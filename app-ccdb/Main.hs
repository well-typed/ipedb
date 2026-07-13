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
import IpeDB.Types.CostCentre qualified as CC
import Options.Applicative qualified as O
import Paths_ipedb (version)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.IO qualified as IO

main :: IO ()
main = do
  O.execParser ccDBOptionsParserInfo >>= \case
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
      DB.withNewTable @CC.CostCentreId @CC.CostCentre session def $ \table -> do
        M.runT_ $
          fromHandle eventlogEncoding eventlogSourceHandle
            ~> decodeEvent
            ~> DB.indexer CC.toCostCentre def{DB.indexerBufferSize = bufferSize} table
        DB.saveTable table ccDBOutputPath ccDBTableFormat

--------------------------------------------------------------------------------
-- Query

runQuery :: QueryOptions -> IO ()
runQuery QueryOptions{..} = do
  DB.withNewSession def $ \session ->
    DB.withTableFrom @CC.CostCentreId @CC.CostCentre session ccDBPath ccDBTableFormat $ \table -> do
      infoProvs <- fmap V.toList . DB.lookups table . V.fromList $ costCentreIds
      for_ (zip costCentreIds infoProvs) $ \(ccId, cc) ->
        putStrLn $ show ccId <> ": " <> show cc

--------------------------------------------------------------------------------
-- List

runList :: ListOptions -> IO ()
runList ListOptions{..} = do
  DB.withNewSession def $ \session ->
    DB.withTableFrom @CC.CostCentreId @CC.CostCentre session ccDBPath ccDBTableFormat $ \table -> do
      DB.withIterator def{DB.iteratorBufferSize = bufferSize} table $ \iterator ->
        M.runT_ $ iterator ~> M.traversing (\(ccId, cc) -> putStrLn $ show ccId <> ": " <> show cc)

--------------------------------------------------------------------------------
-- Check

data MissingCostCentreEntry = MissingCostCentreEntry
  { table1Path :: !FilePath
  , table2Path :: !FilePath
  , costCentreId :: !CC.CostCentreId
  }
  deriving (Show)

instance Exception MissingCostCentreEntry where
  displayException :: MissingCostCentreEntry -> String
  displayException = flip showsException ""
   where
    showsException :: MissingCostCentreEntry -> ShowS
    showsException MissingCostCentreEntry{..} =
      shows costCentreId <> showString ": missing in " <> showString table2Path

data MismatchedCostCentreEntry = MismatchedCostCentreEntry
  { table1Path :: !FilePath
  , table2Path :: !FilePath
  , costCentreId :: !CC.CostCentreId
  , infoProv1 :: !CC.CostCentre
  , infoProv2 :: !CC.CostCentre
  }
  deriving (Show)

instance Exception MismatchedCostCentreEntry where
  displayException :: MismatchedCostCentreEntry -> String
  displayException = flip showsException ""
   where
    showsException :: MismatchedCostCentreEntry -> ShowS
    showsException MismatchedCostCentreEntry{..} =
      shows costCentreId <> showString ": mismatch between " <> showString table1Path <> showString " and " <> showString table2Path

runCheck :: CheckOptions -> IO ()
runCheck CheckOptions{..} = do
  DB.withNewSession def $ \session ->
    DB.withTableFrom @CC.CostCentreId @CC.CostCentre session ccDB1Path ccDB1TableFormat $ \table1 ->
      DB.withTableFrom @CC.CostCentreId @CC.CostCentre session ccDB2Path ccDB2TableFormat $ \table2 -> do
        let runCheck' = \case
              CheckSubset -> checkSubset
              CheckEqual -> checkEqual
        exceptionCount <- runCheck' check (ccDB1Path, table1) (ccDB2Path, table2)
        unless (exceptionCount == 0) $
          exitWith (ExitFailure exceptionCount)
 where
  checkEqual ::
    (FilePath, DB.Table CC.CostCentreId CC.CostCentre) ->
    (FilePath, DB.Table CC.CostCentreId CC.CostCentre) ->
    IO Int
  checkEqual table1Info table2Info = do
    exceptionCount1 <- checkSubset' True table1Info table2Info
    exceptionCount2 <- checkSubset' False table2Info table1Info
    pure (exceptionCount1 + exceptionCount2)

  checkSubset ::
    (FilePath, DB.Table CC.CostCentreId CC.CostCentre) ->
    (FilePath, DB.Table CC.CostCentreId CC.CostCentre) ->
    IO Int
  checkSubset = checkSubset' True

  checkSubset' ::
    Bool ->
    (FilePath, DB.Table CC.CostCentreId CC.CostCentre) ->
    (FilePath, DB.Table CC.CostCentreId CC.CostCentre) ->
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
    resolve :: [(CC.CostCentreId, CC.CostCentre)] -> IO [(CC.CostCentreId, CC.CostCentre, Maybe CC.CostCentre)]
    resolve entries1 = do
      maybeCostCentres2 <- DB.lookups table2 (V.fromList (fst <$> entries1))
      let addToEntry (costCentreId, infoProv1) maybeCostCentre2 = (costCentreId, infoProv1, maybeCostCentre2)
      pure $ zipWith addToEntry entries1 (V.toList maybeCostCentres2)

    assertEqual :: (CC.CostCentreId, CC.CostCentre, Maybe CC.CostCentre) -> Maybe SomeException
    assertEqual (costCentreId, infoProv1, maybeCostCentre2) =
      case maybeCostCentre2 of
        Nothing ->
          Just $ toException MissingCostCentreEntry{..}
        Just infoProv2
          | checkPresentAndEqual && infoProv1 /= infoProv2 ->
              Just $ toException MismatchedCostCentreEntry{..}
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
-- CCDB Options
--------------------------------------------------------------------------------

data CCDBOptions
  = IndexCommand IndexOptions
  | QueryCommand QueryOptions
  | ListCommand ListOptions
  | CheckCommand CheckOptions

ccDBOptionsParserInfo :: O.ParserInfo CCDBOptions
ccDBOptionsParserInfo =
  O.info (ccDBOptionsParser O.<**> O.helper O.<**> ccDBVersioner) . mconcat $
    [ O.progDesc "Build and query cost-centre databases."
    ]

ccDBVersioner :: O.Parser (a -> a)
ccDBVersioner =
  O.simpleVersioner $! "ccdb " <> V.showVersion version

ccDBOptionsParser :: O.Parser CCDBOptions
ccDBOptionsParser =
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
  , ccDBOutputPath :: !FilePath
  , ccDBTableFormat :: !DB.TableFormat
  , bufferSize :: !Word32
  }

indexOptionsParserInfo :: O.ParserInfo IndexOptions
indexOptionsParserInfo =
  O.info (indexOptionsParser O.<**> O.helper) . mconcat $
    [ O.progDesc "Build a cost-centre database from an eventlog."
    ]

indexOptionsParser :: O.Parser IndexOptions
indexOptionsParser =
  IndexOptions
    <$> eventlogSourceParser
    <*> eventlogEncodingParser
    <*> ccDBOutputPathParser
    <*> tableFormatParser
    <*> indexBufferSizeParser

--------------------------------------------------------------------------------
-- Query Options

data QueryOptions = QueryOptions
  { ccDBPath :: !FilePath
  , ccDBTableFormat :: !DB.TableFormat
  , costCentreIds :: ![CC.CostCentreId]
  }

queryOptionsParserInfo :: O.ParserInfo QueryOptions
queryOptionsParserInfo =
  O.info (queryOptionsParser O.<**> O.helper) . mconcat $
    [ O.progDesc "Query a cost-centre database with cost-centre pointers."
    ]

queryOptionsParser :: O.Parser QueryOptions
queryOptionsParser =
  QueryOptions
    <$> ccDBPathParser
    <*> tableFormatParser
    <*> O.some costCentreIdParser

--------------------------------------------------------------------------------
-- List Options

data ListOptions = ListOptions
  { ccDBPath :: !FilePath
  , ccDBTableFormat :: !DB.TableFormat
  , bufferSize :: !Word32
  }

listOptionsParserInfo :: O.ParserInfo ListOptions
listOptionsParserInfo =
  O.info (listOptionsParser O.<**> O.helper) . mconcat $
    [ O.progDesc "List all cost-centre entries in a cost-centre database."
    ]

listOptionsParser :: O.Parser ListOptions
listOptionsParser =
  ListOptions
    <$> ccDBPathParser
    <*> tableFormatParser
    <*> listBufferSizeParser

--------------------------------------------------------------------------------
-- Check Options

data CheckOptions = CheckOptions
  { check :: !Check
  , ccDB1Path :: !FilePath
  , ccDB1TableFormat :: !DB.TableFormat
  , ccDB2Path :: !FilePath
  , ccDB2TableFormat :: !DB.TableFormat
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
    <*> ccDBPathParser
    <*> table1FormatParser
    <*> ccDBPathParser
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
-- CCDB

ccDBOutputPathParser :: O.Parser FilePath
ccDBOutputPathParser =
  O.strOption . mconcat $
    [ O.short 'o'
    , O.long "output"
    , O.metavar "CCDB_PATH"
    , O.completer (O.bashCompleter "file")
    , O.help "The cost-centre database output path."
    ]

ccDBPathParser :: O.Parser FilePath
ccDBPathParser =
  O.strArgument . mconcat $
    [ O.metavar "CCDB_PATH"
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
    , O.help "The cost-centre database format (lsm, tar, tgz)."
    , O.value def
    , mods
    ]

--------------------------------------------------------------------------------
-- Buffer Size

indexBufferSizeParser :: O.Parser Word32
indexBufferSizeParser =
  bufferSizeParser . mconcat $
    [ O.help "The size of the index buffer in number of elements."
    , O.value DB.defaultIndexerOptions.indexerBufferSize
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
-- CostCentreId

costCentreIdParser :: O.Parser CC.CostCentreId
costCentreIdParser =
  O.argument O.auto . mconcat $
    [ O.metavar "CCID"
    ]
