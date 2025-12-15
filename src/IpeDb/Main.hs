{-# LANGUAGE OverloadedStrings #-}

module IpeDb.Main (defaultMain) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Database.SQLite.Simple as Sqlite
import Database.SQLite.Simple.Types (Only (..))
import GHC.RTS.Events (Data (..), EventLog (..), readEventLogFromFile)
import IpeDb.Eventlog.Index
import IpeDb.InfoProv
import qualified IpeDb.Options as Opts
import IpeDb.Table
import IpeDb.Types

defaultMain :: IO ()
defaultMain = do
  opts <- Opts.parseOptions
  case opts.command of
    Opts.Query query -> withInfoProvDb opts.databaseFp $ \db -> do
      lookupInfoProv db query.ipeId >>= \case
        Nothing -> Text.putStrLn $ "No Info Prov found for " <> Text.show query.ipeId
        Just prov -> Text.putStrLn $ prettyInfoProv prov
    Opts.Index index -> withInfoProvDb opts.databaseFp $ \db -> do
      generateInfoProvDb db index.eventlog

prettyInfoProv :: InfoProv -> Text
prettyInfoProv prov =
  Text.unlines
    [ "Info Table  " <> prettyIpeId prov.infoId,
      "  Table Name:      " <> prov.tableName,
      "  Closure Desc:    " <> Text.show prov.closureDesc,
      "  Type Desc:       " <> prov.typeDesc,
      "  Label:           " <> prov.label,
      "  Modulename:      " <> prov.moduleName,
      "  Source Location: " <> prov.srcLoc
    ]

-- ----------------------------------------------------------------------------
-- High Level API
-- ----------------------------------------------------------------------------

withInfoProvDb :: FilePath -> (InfoProvDb -> IO a) -> IO a
withInfoProvDb fp act = Sqlite.withConnection fp $ \conn ->
  act (InfoProvDb {conn})

generateInfoProvDb :: InfoProvDb -> FilePath -> IO ()
generateInfoProvDb db fp = do
  Sqlite.withExclusiveTransaction db.conn $ setupDb db.conn
  setupIndexing db.conn
  readEventLogFromFile fp >>= \case
    Left err -> fail err
    Right (EventLog _h (Data es)) -> Sqlite.withExclusiveTransaction db.conn $ do
      insertInfoProvData db.conn es

lookupInfoProv :: InfoProvDb -> IpeId -> IO (Maybe InfoProv)
lookupInfoProv db ipeId = do
  r <- Sqlite.query db.conn findInfoTableQuery (Only ipeId)
  case r of
    [ipe] -> pure $ Just ipe
    _ -> pure $ Nothing
