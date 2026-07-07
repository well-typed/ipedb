module Main (main) where

import Codec.Compression.GZip qualified as GZip
import Control.Applicative (asum)
import Control.Exception (Exception (..), SomeException)
import Control.Monad (unless)
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
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.IO qualified as IO

main :: IO ()
main = do
  O.execParser ipeDBOptionsParserInfo >>= \case
    IndexCommand options -> runIndex options
    QueryCommand options -> runQuery options
    ListCommand options -> runList options
    CheckCommand options -> runCheck options

--------------------------------------------------------------------------------
-- Index

runIndex :: IndexOptions -> IO ()
runIndex IndexOptions{..} = do
  withEventlogSource eventlogSource $ \eventlogSourceHandle -> do
    DB.withNewSession def $ \session ->
      DB.withNewTable @IP.InfoProvId @IP.InfoProv session def $ \table -> do
        M.runT_ $
          fromHandle eventlogEncoding eventlogSourceHandle
            ~> decodeEvent
            ~> DB.indexer def{DB.indexerBufferSize = bufferSize} table
        DB.saveTable table ipeDBOutputPath ipeDBTableFormat

--------------------------------------------------------------------------------
-- Query

runQuery :: QueryOptions -> IO ()
runQuery QueryOptions{..} = do
  DB.withNewSession def $ \session ->
    DB.withTableFrom @IP.InfoProvId @IP.InfoProv session def ipeDBPath ipeDBTableFormat $ \table -> do
      infoProvs <- fmap V.toList . DB.lookups table . V.fromList $ infoProvIds
      for_ (zip infoProvIds infoProvs) $ \(ipId, ip) ->
        putStrLn $ show ipId <> ": " <> show ip

--------------------------------------------------------------------------------
-- List

runList :: ListOptions -> IO ()
runList ListOptions{..} = do
  DB.withNewSession def $ \session ->
    DB.withTableFrom @IP.InfoProvId @IP.InfoProv session def ipeDBPath ipeDBTableFormat $ \table -> do
      DB.withIterator def{DB.iteratorBufferSize = bufferSize} table $ \iterator ->
        M.runT_ $ iterator ~> M.traversing (\(ipId, ip) -> putStrLn $ show ipId <> ": " <> show ip)

--------------------------------------------------------------------------------
-- Check

data MissingInfoProvEntry = MissingInfoProvEntry
  { table1Path :: !FilePath
  , table2Path :: !FilePath
  , infoProvId :: !IP.InfoProvId
  }
  deriving (Show)

instance Exception MissingInfoProvEntry where
  displayException :: MissingInfoProvEntry -> String
  displayException = flip showsException ""
   where
    showsException :: MissingInfoProvEntry -> ShowS
    showsException MissingInfoProvEntry{..} =
      shows infoProvId <> showString ": missing in " <> showString table2Path

data MismatchedInfoProvEntry = MismatchedInfoProvEntry
  { table1Path :: !FilePath
  , table2Path :: !FilePath
  , infoProvId :: !IP.InfoProvId
  , infoProv1 :: !IP.InfoProv
  , infoProv2 :: !IP.InfoProv
  }
  deriving (Show)

instance Exception MismatchedInfoProvEntry where
  displayException :: MismatchedInfoProvEntry -> String
  displayException = flip showsException ""
   where
    showsException :: MismatchedInfoProvEntry -> ShowS
    showsException MismatchedInfoProvEntry{..} =
      shows infoProvId <> showString ": mismatch between " <> showString table1Path <> showString " and " <> showString table2Path

runCheck :: CheckOptions -> IO ()
runCheck CheckOptions{..} = do
  DB.withNewSession def $ \session ->
    DB.withTableFrom @IP.InfoProvId @IP.InfoProv session def ipeDB1Path ipeDB1TableFormat $ \table1 ->
      DB.withTableFrom @IP.InfoProvId @IP.InfoProv session def ipeDB2Path ipeDB2TableFormat $ \table2 -> do
        let runCheck' = \case
              CheckSubset -> checkSubset
              CheckEqual -> checkEqual
        exceptionCount <- runCheck' check (ipeDB1Path, table1) (ipeDB2Path, table2)
        unless (exceptionCount == 0) $
          exitWith (ExitFailure exceptionCount)
 where
  checkEqual ::
    (FilePath, DB.Table IP.InfoProvId IP.InfoProv) ->
    (FilePath, DB.Table IP.InfoProvId IP.InfoProv) ->
    IO Int
  checkEqual table1Info table2Info = do
    exceptionCount1 <- checkSubset' True table1Info table2Info
    exceptionCount2 <- checkSubset' False table2Info table1Info
    pure (exceptionCount1 + exceptionCount2)

  checkSubset ::
    (FilePath, DB.Table IP.InfoProvId IP.InfoProv) ->
    (FilePath, DB.Table IP.InfoProvId IP.InfoProv) ->
    IO Int
  checkSubset = checkSubset' True

  checkSubset' ::
    Bool ->
    (FilePath, DB.Table IP.InfoProvId IP.InfoProv) ->
    (FilePath, DB.Table IP.InfoProvId IP.InfoProv) ->
    IO Int
  checkSubset' checkPresentAndEqual (table1Path, table1) (table2Path, table2) =
    DB.withIterator def{DB.iteratorBufferSize = bufferSize} table1 $ \iterator1 -> do
      fmap sum . M.runT $
        iterator1
          ~> M.buffered (fromIntegral bufferSize)
          ~> M.traversing resolve
          ~> M.asParts
          ~> M.mapping assertEqual
          ~> M.asParts
          ~> M.traversing (\e -> IO.hPutStr IO.stderr (displayException e) >> pure (1 :: Int))
   where
    resolve :: [(IP.InfoProvId, IP.InfoProv)] -> IO [(IP.InfoProvId, IP.InfoProv, Maybe IP.InfoProv)]
    resolve entries1 = do
      maybeInfoProvs2 <- DB.lookups table2 (V.fromList (fst <$> entries1))
      let addToEntry (infoProvId, infoProv1) maybeInfoProv2 = (infoProvId, infoProv1, maybeInfoProv2)
      pure $ zipWith addToEntry entries1 (V.toList maybeInfoProvs2)

    assertEqual :: (IP.InfoProvId, IP.InfoProv, Maybe IP.InfoProv) -> Maybe SomeException
    assertEqual (infoProvId, infoProv1, maybeInfoProv2) =
      case maybeInfoProv2 of
        Nothing ->
          Just $ toException MissingInfoProvEntry{..}
        Just infoProv2
          | checkPresentAndEqual && infoProv1 /= infoProv2 ->
              Just $ toException MismatchedInfoProvEntry{..}
          | otherwise -> Nothing

--------------------------------------------------------------------------------
-- Eventlog Processing
--------------------------------------------------------------------------------

{- |
Stream a file as chunks.
-}
fromHandle :: EventlogEncoding -> IO.Handle -> M.SourceT IO BS.ByteString
fromHandle eventlogEncoding h =
  M.MachineT $ do
    chunks <- liftIO (BSL.toChunks . decodeEventlog eventlogEncoding <$> BSL.hGetContents h)
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
  | CheckCommand CheckOptions

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
    , O.command "check" (CheckCommand <$> checkOptionsParserInfo)
    ]

--------------------------------------------------------------------------------
-- Index Options

data IndexOptions = IndexOptions
  { eventlogSource :: EventlogSource
  , eventlogEncoding :: EventlogEncoding
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
    <*> eventlogEncodingParser
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
-- Check Options

data CheckOptions = CheckOptions
  { check :: !Check
  , ipeDB1Path :: !FilePath
  , ipeDB1TableFormat :: !DB.TableFormat
  , ipeDB2Path :: !FilePath
  , ipeDB2TableFormat :: !DB.TableFormat
  , bufferSize :: !Word32
  }

checkOptionsParserInfo :: O.ParserInfo CheckOptions
checkOptionsParserInfo =
  O.info (checkOptionsParser O.<**> O.helper) . mconcat $
    [ O.progDesc "Compare two tables."
    ]

checkOptionsParser :: O.Parser CheckOptions
checkOptionsParser =
  CheckOptions
    <$> checkParser
    <*> ipeDBPathParser
    <*> table1FormatParser
    <*> ipeDBPathParser
    <*> table2FormatParser
    <*> listBufferSizeParser

data Check
  = CheckSubset
  | CheckEqual

checkParser :: O.Parser Check
checkParser =
  asum
    [ O.flag' CheckSubset . mconcat $
        [ O.long "check-subset"
        , O.help "Check that the first table is a subset of the second table."
        ]
    , O.flag' CheckEqual . mconcat $
        [ O.long "check-equal"
        , O.help "Check that the first table and the second table are equal."
        ]
    , pure CheckEqual
    ]

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

data EventlogEncoding
  = EventlogEncodingNone
  | EventlogEncodingGZip

readEventlogEncoding :: String -> Either String EventlogEncoding
readEventlogEncoding = \case
  "none" -> Right EventlogEncodingNone
  "gzip" -> Right EventlogEncodingGZip
  eventlogEncodingString -> Left $! "Unknown eventlog encoding " <> eventlogEncodingString

eventlogEncodingParser :: O.Parser EventlogEncoding
eventlogEncodingParser =
  O.option (O.eitherReader readEventlogEncoding) . mconcat $
    [ O.short 'e'
    , O.long "eventlog-encoding"
    , O.metavar "FORMAT"
    , O.completeWith ["none", "gzip"]
    , O.help "The eventlog encoding (none, gzip)."
    , O.value EventlogEncodingNone
    ]

decodeEventlog :: EventlogEncoding -> BSL.ByteString -> BSL.ByteString
decodeEventlog = \case
  EventlogEncodingNone -> id
  EventlogEncodingGZip -> GZip.decompress

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
  genericTableFormatParser . mconcat $
    [ O.short 't'
    , O.long "table-format"
    ]

table1FormatParser :: O.Parser DB.TableFormat
table1FormatParser =
  genericTableFormatParser $
    O.long "table1-format"

table2FormatParser :: O.Parser DB.TableFormat
table2FormatParser =
  genericTableFormatParser $
    O.long "table2-format"

genericTableFormatParser :: O.Mod O.OptionFields DB.TableFormat -> O.Parser DB.TableFormat
genericTableFormatParser mods =
  O.option (O.eitherReader readTableFormat) . mconcat $
    [ O.metavar "FORMAT"
    , O.completeWith ["lsm", "tar", "tgz"]
    , O.help "The IpeDB table format (lsm, tar, tgz)."
    , O.value def
    , mods
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
