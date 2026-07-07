module Main (main) where

import Codec.Compression.GZip qualified as GZip
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Default (Default (..))
import Data.Foldable (for_)
import Data.Machine ((~>))
import Data.Machine qualified as M
import Data.String (IsString (..))
import Data.Vector qualified as V
import Data.Version qualified as V (showVersion)
import Data.Word (Word32)
import GHC.RTS.Events (Event)
import GHC.RTS.Events.Incremental (Decoder (..), decodeEventLog)
import IpeDB.Database qualified as DB
import IpeDB.Types.InfoProv qualified as IP
import Options.Applicative qualified as O
import Paths_ipedb (version)
import System.Exit (exitFailure)
import System.IO qualified as IO

main :: IO ()
main = do
  O.execParser ipeDBOptionsParserInfo >>= \case
    IndexCommand options -> runIndex options
    QueryCommand options -> runQuery options
    ListCommand options -> runList options

runIndex :: IndexOptions -> IO ()
runIndex IndexOptions{..} = do
  withEventlogSource eventlogSource $ \eventlogSourceHandle -> do
    DB.withNewSession def $ \session ->
      DB.withNewTable @IP.InfoProvId @IP.InfoProv session def $ \table -> do
        M.runT_ $
          fromHandle eventlogFormat eventlogSourceHandle
            ~> decodeEvent
            ~> DB.indexer def{DB.indexerBufferSize = bufferSize} table
        DB.saveTable table ipeDBOutputPath ipeDBTableFormat

runQuery :: QueryOptions -> IO ()
runQuery QueryOptions{..} = do
  DB.withNewSession def $ \session ->
    DB.withTableFrom @IP.InfoProvId @IP.InfoProv session def ipeDBPath ipeDBTableFormat $ \table -> do
      infoProvs <- fmap V.toList . DB.lookups table . V.fromList $ infoProvIds
      for_ (zip infoProvIds infoProvs) $ \(ipId, ip) ->
        putStrLn $ show ipId <> ": " <> show ip

runList :: ListOptions -> IO ()
runList ListOptions{..} = do
  DB.withNewSession def $ \session ->
    DB.withTableFrom @IP.InfoProvId @IP.InfoProv session def ipeDBPath ipeDBTableFormat $ \table -> do
      DB.withIterator def{DB.iteratorBufferSize = bufferSize} table $ \iterator ->
        M.runT_ $ iterator ~> M.traversing (\(ipId, ip) -> putStrLn $ show ipId <> ": " <> show ip)

--------------------------------------------------------------------------------
-- Eventlog Processing
--------------------------------------------------------------------------------

{- |
Stream a file as chunks.
-}
fromHandle :: EventlogFormat -> IO.Handle -> M.SourceT IO BS.ByteString
fromHandle eventlogFormat h =
  M.MachineT $ do
    chunks <- liftIO (BSL.toChunks . toBinaryEventlog eventlogFormat <$> BSL.hGetContents h)
    M.runMachineT $ M.source chunks

{- |
Parse t`Event`s from a stream of `BS.ByteString` chunks.
-}
decodeEvent :: M.Process BS.ByteString Event
decodeEvent = M.construct $ loop decodeEventLog
 where
  loop :: Decoder a -> M.PlanT (M.Is BS.ByteString) a m ()
  loop Done{} = pure ()
  loop (Consume k) = M.await >>= \chunk -> loop (k chunk)
  loop (Produce a d') = M.yield a >> loop d'
  loop (Error _ err) = error err

--------------------------------------------------------------------------------
-- IpeDB Options
--------------------------------------------------------------------------------

data IpeDBOptions
  = IndexCommand IndexOptions
  | QueryCommand QueryOptions
  | ListCommand ListOptions

ipeDBOptionsParserInfo :: O.ParserInfo IpeDBOptions
ipeDBOptionsParserInfo =
  O.info (ipeDBOptionsParser O.<**> O.helper O.<**> ipeDBVersioner) . mconcat $
    [ O.progDesc "Build and query IPE databases."
    ]

ipeDBVersioner :: O.Parser (a -> a)
ipeDBVersioner =
  O.simpleVersioner $! "ipedb " <> V.showVersion version

ipeDBOptionsParser :: O.Parser IpeDBOptions
ipeDBOptionsParser =
  O.subparser . mconcat $
    [ O.command "index" (IndexCommand <$> indexOptionsParserInfo)
    , O.command "query" (QueryCommand <$> queryOptionsParserInfo)
    , O.command "list" (ListCommand <$> listOptionsParserInfo)
    ]

--------------------------------------------------------------------------------
-- Index Options

data IndexOptions = IndexOptions
  { eventlogSource :: EventlogSource
  , eventlogFormat :: EventlogFormat
  , ipeDBOutputPath :: !FilePath
  , ipeDBTableFormat :: !DB.TableFormat
  , bufferSize :: !Word32
  }

indexOptionsParserInfo :: O.ParserInfo IndexOptions
indexOptionsParserInfo =
  O.info (indexOptionsParser O.<**> O.helper) . mconcat $
    [ O.progDesc "Build an IPE database from an eventlog."
    ]

indexOptionsParser :: O.Parser IndexOptions
indexOptionsParser =
  IndexOptions
    <$> eventlogSourceParser
    <*> eventlogFormatParser
    <*> ipeDBOutputPathParser
    <*> tableFormatParser
    <*> indexBufferSizeParser

--------------------------------------------------------------------------------
-- Query Options

data QueryOptions = QueryOptions
  { ipeDBPath :: !FilePath
  , ipeDBTableFormat :: !DB.TableFormat
  , infoProvIds :: ![IP.InfoProvId]
  }

queryOptionsParserInfo :: O.ParserInfo QueryOptions
queryOptionsParserInfo =
  O.info (queryOptionsParser O.<**> O.helper) . mconcat $
    [ O.progDesc "Query an IPE database with IPE pointers."
    ]

queryOptionsParser :: O.Parser QueryOptions
queryOptionsParser =
  QueryOptions
    <$> ipeDBPathParser
    <*> tableFormatParser
    <*> O.some infoProvPtrParser

--------------------------------------------------------------------------------
-- List Options

data ListOptions = ListOptions
  { ipeDBPath :: !FilePath
  , ipeDBTableFormat :: !DB.TableFormat
  , bufferSize :: !Word32
  }

listOptionsParserInfo :: O.ParserInfo ListOptions
listOptionsParserInfo =
  O.info (listOptionsParser O.<**> O.helper) . mconcat $
    [ O.progDesc "List all IPE entries in an IPE database."
    ]

listOptionsParser :: O.Parser ListOptions
listOptionsParser =
  ListOptions
    <$> ipeDBPathParser
    <*> tableFormatParser
    <*> listBufferSizeParser

--------------------------------------------------------------------------------
-- Eventlog Source

data EventlogSource
  = EventlogFromStdin
  | EventlogFromFile {file :: !FilePath}

instance IsString EventlogSource where
  fromString :: String -> EventlogSource
  fromString x = if x == "-" then EventlogFromStdin else EventlogFromFile x

eventlogSourceParser :: O.Parser EventlogSource
eventlogSourceParser =
  O.strArgument . mconcat $
    [ O.metavar "EVENTLOG"
    , O.completer (O.bashCompleter "file")
    ]

withEventlogSource :: EventlogSource -> (IO.Handle -> IO a) -> IO a
withEventlogSource EventlogFromStdin action = do
  stdinIsTerminalDevice <- IO.hIsTerminalDevice IO.stdin
  if stdinIsTerminalDevice
    then do
      putStrLn "Cannot read eventlog from terminal"
      exitFailure
    else do
      IO.hSetBinaryMode IO.stdin True
      action IO.stdin
withEventlogSource EventlogFromFile{..} action =
  IO.withBinaryFile file IO.ReadMode action

--------------------------------------------------------------------------------
-- Eventlog Format

data EventlogFormat
  = EventlogFormatBinary
  | EventlogFormatBinaryGZip

readEventlogFormat :: String -> Either String EventlogFormat
readEventlogFormat = \case
  "bin" -> Right EventlogFormatBinary
  "bgz" -> Right EventlogFormatBinaryGZip
  eventlogFormatString -> Left $! "Unknown eventlog format " <> eventlogFormatString

eventlogFormatParser :: O.Parser EventlogFormat
eventlogFormatParser =
  O.option (O.eitherReader readEventlogFormat) . mconcat $
    [ O.short 'e'
    , O.long "eventlog-format"
    , O.metavar "FORMAT"
    , O.completeWith ["bin", "bgz"]
    , O.help "The eventlog format (bin, bgz)."
    , O.value EventlogFormatBinary
    ]

toBinaryEventlog :: EventlogFormat -> BSL.ByteString -> BSL.ByteString
toBinaryEventlog = \case
  EventlogFormatBinary -> id
  EventlogFormatBinaryGZip -> GZip.decompress

--------------------------------------------------------------------------------
-- IpeDB

ipeDBOutputPathParser :: O.Parser FilePath
ipeDBOutputPathParser =
  O.strOption . mconcat $
    [ O.short 'o'
    , O.long "output"
    , O.metavar "IPEDB_PATH"
    , O.completer (O.bashCompleter "file")
    , O.help "The IpeDB output path."
    ]

ipeDBPathParser :: O.Parser FilePath
ipeDBPathParser =
  O.strArgument . mconcat $
    [ O.metavar "IPEDB_PATH"
    , O.completer (O.bashCompleter "file")
    ]

--------------------------------------------------------------------------------
-- TableFormat

readTableFormat :: String -> Either String DB.TableFormat
readTableFormat = \case
  "lsm" -> Right DB.LSMTreeSnapshotV2
  "tar" -> Right DB.LSMTreeSnapshotV2Tar
  "tgz" -> Right DB.LSMTreeSnapshotV2TarGz
  tableFormatString -> Left $! "Unknown table format " <> tableFormatString

tableFormatParser :: O.Parser DB.TableFormat
tableFormatParser =
  O.option (O.eitherReader readTableFormat) . mconcat $
    [ O.short 't'
    , O.long "table-format"
    , O.metavar "FORMAT"
    , O.completeWith ["lsm", "tar", "tgz"]
    , O.help "The IpeDB table format (lsm, tar, tgz)."
    , O.value def
    ]

--------------------------------------------------------------------------------
-- Buffer Size

indexBufferSizeParser :: O.Parser Word32
indexBufferSizeParser =
  bufferSizeParser . mconcat $
    [ O.help "The size of the index buffer in number of elements."
    , O.value IP.defaultInfoProvIndexerOptions.indexerBufferSize
    ]

listBufferSizeParser :: O.Parser Word32
listBufferSizeParser =
  bufferSizeParser . mconcat $
    [ O.help "The size of the lookup buffer in number of elements."
    , O.value DB.defaultIteratorOptions.iteratorBufferSize
    ]

bufferSizeParser :: O.Mod O.OptionFields Word32 -> O.Parser Word32
bufferSizeParser mods =
  O.option O.auto . mconcat $
    [ O.long "buffer-size"
    , O.metavar "N"
    , mods
    ]

--------------------------------------------------------------------------------
-- InfoProvId

infoProvPtrParser :: O.Parser IP.InfoProvId
infoProvPtrParser =
  O.argument O.auto . mconcat $
    [ O.metavar "IPE_PTR"
    ]
