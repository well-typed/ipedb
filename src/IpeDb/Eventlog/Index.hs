{-# LANGUAGE OverloadedStrings #-}

module IpeDb.Eventlog.Index (
  generateInfoProvDb,
  setupDb,
  setupIndexing,
  insertInfoProvData,
  insertInfoProv,
  upsertInfoProvStrings,
)
where

import Data.Foldable (traverse_)
import qualified Database.SQLite.Simple as Sqlite
import Database.SQLite.Simple.Types (Only (..))
import qualified GHC.RTS.Events as GhcEvents
import IpeDb.InfoProv as Ipe
import IpeDb.Table as Table
import IpeDb.Types

insertInfoProvData :: (Foldable t) => Sqlite.Connection -> t GhcEvents.Event -> IO ()
insertInfoProvData conn es = traverse_ (processIpeEvents conn) es

generateInfoProvDb :: InfoProvDb -> FilePath -> IO ()
generateInfoProvDb db fp = do
  Sqlite.withExclusiveTransaction db.conn $ setupDb db.conn
  setupIndexing db.conn
  GhcEvents.readEventLogFromFile fp >>= \case
    Left err -> fail err
    Right (GhcEvents.EventLog _h (GhcEvents.Data es)) -> Sqlite.withExclusiveTransaction db.conn $ do
      insertInfoProvData db.conn es

-- ----------------------------------------------------------------------------
-- Low Level Sqlite api
-- ----------------------------------------------------------------------------

setupDb :: Sqlite.Connection -> IO ()
setupDb conn = do
  Sqlite.execute_ conn dropStringTableStmt
  Sqlite.execute_ conn dropInfoProvTableStmt
  Sqlite.execute_ conn dropInfoProvTableViewStmt
  Sqlite.execute_ conn stringTableStmt
  Sqlite.execute_ conn infoProvTableStmt
  Sqlite.execute_ conn infoProvTableViewStmt

setupIndexing :: Sqlite.Connection -> IO ()
setupIndexing conn = do
  Sqlite.execute_ conn "PRAGMA synchronous = OFF;"
  Sqlite.execute_ conn "PRAGMA journal_mode = OFF;"
  Sqlite.execute_ conn "PRAGMA temp_store = MEMORY;"
  Sqlite.execute_ conn "PRAGMA locking_mode = EXCLUSIVE;"

insertInfoProv :: Sqlite.Connection -> InfoProv -> IO ()
insertInfoProv conn prov = do
  row <- upsertInfoProvStrings conn prov
  _ <- Sqlite.execute conn insertInfoTableQuery row
  pure ()

upsertInfoProvStrings :: Sqlite.Connection -> InfoProv -> IO InfoProvRow
upsertInfoProvStrings conn prov = do
  Sqlite.executeMany
    conn
    insertOrIgnoreString
    [ Only prov.tableName
    , Only prov.typeDesc
    , Only prov.label
    , Only prov.moduleName
    , Only prov.srcLoc
    ]
  [(taId, tyId, labelId, modId, srcLocId)] <-
    Sqlite.query
      conn
      getIpeStrings
      ( prov.typeDesc
      , prov.label
      , prov.moduleName
      , prov.srcLoc
      , prov.tableName
      )
  pure
    InfoProvRow
      { Table.infoId = prov.infoId
      , Table.tableName = taId
      , Table.closureDesc = prov.closureDesc
      , Table.typeDesc = tyId
      , Table.label = labelId
      , Table.moduleName = modId
      , Table.srcLoc = srcLocId
      }

-- ----------------------------------------------------------------------------
-- Eventlog processing
-- ----------------------------------------------------------------------------

processIpeEvents :: Sqlite.Connection -> GhcEvents.Event -> IO ()
processIpeEvents conn ev = case eventInfoToInfoProv (GhcEvents.evSpec ev) of
  Nothing -> pure ()
  Just infoProv -> insertInfoProv conn infoProv

eventInfoToInfoProv :: GhcEvents.EventInfo -> Maybe InfoProv
eventInfoToInfoProv ev = case ev of
  it@GhcEvents.InfoTableProv{} ->
    Just
      InfoProv
        { Ipe.infoId = IpeId $ GhcEvents.itInfo it
        , Ipe.tableName = GhcEvents.itTableName it
        , Ipe.closureDesc = fromIntegral $ GhcEvents.itClosureDesc it
        , Ipe.typeDesc = GhcEvents.itTyDesc it
        , Ipe.label = GhcEvents.itLabel it
        , Ipe.moduleName = GhcEvents.itModule it
        , Ipe.srcLoc = GhcEvents.itSrcLoc it
        }
  _ -> Nothing
