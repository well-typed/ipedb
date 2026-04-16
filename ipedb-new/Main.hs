{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Binary as B
import qualified Data.ByteString as BS
import Data.Machine ((~>))
import qualified Data.Machine as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as VM
import Data.Void (Void)
import Data.Word (Word64)
import qualified Database.LSMTree as LSMT
import qualified GHC.RTS.Events as E
import qualified GHC.RTS.Events.Incremental as EI
import GHC.Records (HasField (..))
import qualified Options.Applicative as O
import qualified System.Directory as D
import System.FilePath ((-<.>))
import System.IO (withFile)
import qualified System.IO as IO

data Options = Options
    { eventlogPath :: FilePath
    , maybeDatabasePath :: Maybe FilePath
    }

instance HasField "databasePath" Options FilePath where
    getField :: Options -> FilePath
    getField opts =
        fromMaybe (opts.eventlogPath -<.> "ipedb") opts.maybeDatabasePath

options :: O.ParserInfo Options
options =
    O.info
        (optionsParser O.<**> O.helper)
        (O.header "ipedb - create and query IPE databases")

optionsParser :: O.Parser Options
optionsParser =
    Options
        <$> O.strArgument
            ( O.metavar "FILE"
                <> O.help "The input .eventlog file."
            )
        <*> O.optional
            ( O.strOption
                ( O.short 'o'
                    <> O.long "output"
                    <> O.metavar "FILE"
                    <> O.help "The output .ipedb file."
                )
            )

main :: IO ()
main = do
    opts <- O.execParser options

    create opts

create :: Options -> IO ()
create opts = do
    -- Ensure databasePath exists and is a directory.
    databasePathExists <- D.doesDirectoryExist opts.databasePath
    when (not databasePathExists) $ D.createDirectory opts.databasePath
    -- Open the database session.
    LSMT.withOpenSessionIO mempty opts.databasePath $ \session ->
        -- Create a new table for IPE information.
        LSMT.withTable session $ \infoTableProvTable -> do
            -- Open the eventlog file.
            withFile opts.eventlogPath IO.ReadMode $ \eventlogHandle -> do
                -- Process the eventlog file.
                M.runT_ $
                    fromHandle eventlogHandle
                        ~> decodeEvent
                        ~> toInfoTableProv
                        ~> M.mapping (\(k, v) -> (k, v, (Nothing :: Maybe Void)))
                        ~> batch 10
                        ~> inserts infoTableProvTable
                LSMT.saveSnapshot "info-table-prov" "InfoTableProv" infoTableProvTable

fromHandle :: IO.Handle -> M.SourceT IO BS.ByteString
fromHandle h = M.construct chunkSource
  where
    chunkSource :: M.PlanT k BS.ByteString IO ()
    chunkSource = do
        chunk <- liftIO (BS.hGetSome h chunkSizeBytes)
        if BS.null chunk
            then pure ()
            else M.yield chunk >> chunkSource

    chunkSizeBytes :: Int
    chunkSizeBytes = 4 * 1024

decodeEvent :: M.ProcessT IO BS.ByteString E.Event
decodeEvent = M.construct $ loop EI.decodeEventLog
  where
    loop :: EI.Decoder a -> M.PlanT (M.Is BS.ByteString) a IO ()
    loop EI.Done{} = pure ()
    loop (EI.Consume k) = M.await >>= \chunk -> loop (k chunk)
    loop (EI.Produce a d') = M.yield a >> loop d'
    loop (EI.Error _ err) = error err

newtype InfoTableProvKey = InfoTableProvKey Word64
    deriving stock (Show)
    deriving newtype (LSMT.SerialiseKey)
    deriving newtype (LSMT.SerialiseKeyOrderPreserving)

data InfoTableProv = InfoTableProv
    { itTableName :: !Text
    , itClosureDesc :: !Int
    , itTyDesc :: !Text
    , itLabel :: !Text
    , itModule :: !Text
    , itSrcLoc :: !Text
    }
    deriving stock (Show)
    deriving (LSMT.ResolveValue) via (LSMT.ResolveAsFirst InfoTableProv)

instance B.Binary InfoTableProv where
    get :: B.Get InfoTableProv
    get =
        InfoTableProv
            <$> getTextUtf8
            <*> B.get
            <*> getTextUtf8
            <*> getTextUtf8
            <*> getTextUtf8
            <*> getTextUtf8
      where
        getTextUtf8 :: B.Get Text
        getTextUtf8 = TE.decodeUtf8 <$> B.get

    put :: InfoTableProv -> B.Put
    put InfoTableProv{..} = do
        B.put (TE.encodeUtf8 itTableName)
        B.put itClosureDesc
        B.put (TE.encodeUtf8 itTyDesc)
        B.put (TE.encodeUtf8 itLabel)
        B.put (TE.encodeUtf8 itModule)
        B.put (TE.encodeUtf8 itSrcLoc)

instance LSMT.SerialiseValue InfoTableProv where
    serialiseValue :: InfoTableProv -> LSMT.RawBytes
    serialiseValue it = LSMT.serialiseValue (B.encode it)

    deserialiseValue :: LSMT.RawBytes -> InfoTableProv
    deserialiseValue rawBytes = B.decode (LSMT.deserialiseValue rawBytes)

toInfoTableProv :: M.Process E.Event (InfoTableProvKey, InfoTableProv)
toInfoTableProv =
    M.repeatedly $ do
        ev <- M.await
        case E.evSpec ev of
            E.InfoTableProv{..} -> do
                let !infoTableProvKey = InfoTableProvKey itInfo
                let !infoTableProv = InfoTableProv{..}
                M.yield (infoTableProvKey, infoTableProv)
            _otherwise -> pure ()

batch :: forall v. Int -> M.ProcessT IO v (Vector v)
batch batchSize = batchAcc Nothing
  where
    batchAcc :: Maybe (Int, IOVector v) -> M.ProcessT IO v (Vector v)
    batchAcc acc = M.MachineT $ pure $ M.Await onNext M.Refl onStop
      where
        onNext v =
            M.MachineT $
                case acc of
                    Nothing -> do
                        vs' <- liftIO (VM.unsafeNew @IO batchSize)
                        liftIO (VM.unsafeWrite vs' 0 v)
                        M.runMachineT $ batchAcc (Just (1, vs'))
                    Just (n, vs)
                        | n < batchSize -> do
                            liftIO (VM.unsafeWrite vs n v)
                            M.runMachineT $ batchAcc (Just (n + 1, vs))
                        | otherwise -> do
                            vs' <- liftIO (VM.unsafeNew @IO batchSize)
                            liftIO (VM.unsafeWrite vs' 0 v)
                            M.runMachineT $ yieldVector (n, vs) $ batchAcc (Just (1, vs'))
        onStop = yieldAcc acc $ M.stopped

    yieldAcc :: Maybe (Int, IOVector v) -> M.ProcessT IO m (Vector v) -> M.ProcessT IO m (Vector v)
    yieldAcc = maybe id yieldVector

    yieldVector :: (Int, IOVector v) -> M.ProcessT IO m (Vector v) -> M.ProcessT IO m (Vector v)
    yieldVector (n, vs) k =
        M.MachineT $
            V.unsafeFreeze @IO (VM.slice 0 n vs) >>= \vs' ->
                pure $ M.Yield vs' k

inserts :: (LSMT.SerialiseKey k, LSMT.SerialiseValue v, LSMT.ResolveValue v, LSMT.SerialiseValue b) => LSMT.Table IO k v b -> M.ProcessT IO (Vector (k, v, Maybe b)) ()
inserts table = M.repeatedly $ do
    M.await >>= \infoTableProvs ->
        liftIO $ do
            LSMT.inserts table infoTableProvs
