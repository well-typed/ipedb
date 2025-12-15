{-# LANGUAGE OverloadedStrings #-}

module IpeDb.Eventlog.Index
  ( setupDb,
      setupIndexing,
      insertInfoProvData,
      insertInfoProv,
      upsertInfoProvStrings,
  )
where

import Data.Foldable (traverse_)
import qualified Database.SQLite.Simple as Sqlite
import Database.SQLite.Simple.Types (Only (..))
import GHC.RTS.Events
  ( Event (..),
  )
import qualified GHC.RTS.Events as Ghc
import IpeDb.InfoProv as Ipe
import IpeDb.Table as Table

insertInfoProvData :: Foldable t => Sqlite.Connection -> t Event -> IO ()
insertInfoProvData conn es = traverse_ (processIpeEvents conn) es

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
    [ Only prov.tableName,
      Only prov.typeDesc,
      Only prov.label,
      Only prov.moduleName,
      Only prov.srcLoc
    ]
  [(taId, tyId, labelId, modId, srcLocId)] <-
    Sqlite.query
      conn
      getIpeStrings
      ( prov.typeDesc,
        prov.label,
        prov.moduleName,
        prov.srcLoc,
        prov.tableName
      )
  pure
    InfoProvRow
      { Table.infoId = prov.infoId,
        Table.tableName = taId,
        Table.closureDesc = prov.closureDesc,
        Table.typeDesc = tyId,
        Table.label = labelId,
        Table.moduleName = modId,
        Table.srcLoc = srcLocId
      }

-- ----------------------------------------------------------------------------
-- Eventlog processing
-- ----------------------------------------------------------------------------

processIpeEvents :: Sqlite.Connection -> Event -> IO ()
processIpeEvents conn ev = case eventInfoToInfoProv (evSpec ev) of
  Nothing -> pure ()
  Just infoProv -> insertInfoProv conn infoProv

eventInfoToInfoProv :: Ghc.EventInfo -> Maybe InfoProv
eventInfoToInfoProv ev = case ev of
  it@Ghc.InfoTableProv {} ->
    Just
      InfoProv
        { Ipe.infoId = IpeId $ Ghc.itInfo it,
          Ipe.tableName = Ghc.itTableName it,
          Ipe.closureDesc = fromIntegral $ Ghc.itClosureDesc it,
          Ipe.typeDesc = Ghc.itTyDesc it,
          Ipe.label = Ghc.itLabel it,
          Ipe.moduleName = Ghc.itModule it,
          Ipe.srcLoc = Ghc.itSrcLoc it
        }
  _ -> Nothing
