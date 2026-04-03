{-# LANGUAGE OverloadedStrings #-}

module IpeDb.Table (
  InfoProvRow (..),
  dropStringTableStmt,
  dropInfoProvTableStmt,
  dropInfoProvTableViewStmt,
  stringTableStmt,
  stringValueIndexStmt,
  infoProvTableStmt,
  infoProvTableViewStmt,
  findInfoTableQuery,
  findAllInfoTablesQuery,
  insertInfoTableQuery,
  insertOrIgnoreString,
  getStringEntry,
  getIpeStrings,
) where

import Data.Int
import qualified Database.SQLite.Simple as Sqlite
import IpeDb.InfoProv
import GHC.Generics (Generic)
import Data.Text (Text)

data InfoProvRow = InfoProvRow
  { infoId :: !IpeId
  , tableName :: !Int64
  , closureDesc :: !Int64
  , typeDesc :: !Int64
  , label :: !Int64
  , moduleName :: !Int64
  , srcLoc :: !Int64
  , srcLocRange :: !Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Sqlite.FromRow, Sqlite.ToRow)

dropStringTableStmt :: Sqlite.Query
dropStringTableStmt = "DROP TABLE IF EXISTS strings;"

dropInfoProvTableStmt :: Sqlite.Query
dropInfoProvTableStmt = "DROP TABLE IF EXISTS info_prov;"

dropInfoProvTableViewStmt :: Sqlite.Query
dropInfoProvTableViewStmt = "DROP VIEW IF EXISTS view_info_prov;"

stringTableStmt :: Sqlite.Query
stringTableStmt =
  "\
  \ CREATE TABLE strings (                        \
  \    id INTEGER PRIMARY KEY,                    \
  \    value TEXT NOT NULL UNIQUE                 \
  \);"

stringValueIndexStmt :: Sqlite.Query
stringValueIndexStmt =
  "\
  \ CREATE UNIQUE INDEX IF NOT EXISTS idx_strings_value \
  \    ON strings(value);                               \
  \"

infoProvTableStmt :: Sqlite.Query
infoProvTableStmt =
  "\
  \ CREATE TABLE info_prov (                                \
  \    info_id         INTEGER PRIMARY KEY,                    \
  \    table_name      INTEGER NOT NULL REFERENCES strings(id),\
  \    closure_desc    INTEGER NOT NULL,                       \
  \    type_desc       INTEGER NOT NULL REFERENCES strings(id),\
  \    label           INTEGER NOT NULL REFERENCES strings(id),\
  \    module_name     INTEGER NOT NULL REFERENCES strings(id),\
  \    src_loc         INTEGER NOT NULL REFERENCES strings(id),\
  \    src_loc_range   TEXT NOT NULL                           \
  \);"

infoProvTableViewStmt :: Sqlite.Query
infoProvTableViewStmt =
  "\
  \ CREATE VIEW view_info_prov AS                                  \
  \ SELECT                                                         \
  \   i.info_id,                                                   \
  \   table_name.value AS table_name,                              \
  \   i.closure_desc,                                              \
  \   type_desc.value AS type_desc,                                \
  \   label.value AS label,                                        \
  \   module_name.value AS module_name,                            \
  \   concat(src_loc.value, i.src_loc_range) AS src_loc            \
  \ FROM info_prov i                                               \
  \ JOIN strings AS table_name   ON i.table_name = table_name.id   \
  \ JOIN strings AS type_desc    ON i.type_desc = type_desc.id     \
  \ JOIN strings AS label        ON i.label = label.id             \
  \ JOIN strings AS module_name  ON i.module_name = module_name.id \
  \ JOIN strings AS src_loc      ON i.src_loc = src_loc.id;"

findInfoTableQuery :: Sqlite.Query
findInfoTableQuery = "SELECT * FROM view_info_prov WHERE info_id = ?;"

findAllInfoTablesQuery :: Sqlite.Query
findAllInfoTablesQuery = "SELECT * FROM view_info_prov;"

insertInfoTableQuery :: Sqlite.Query
insertInfoTableQuery =
  "\
  \ INSERT INTO info_prov\
  \   (info_id, table_name, closure_desc, type_desc, label, module_name, src_loc, src_loc_range)\
  \ VALUES (?, ?, ?, ?, ?, ?, ?, ?);"

insertOrIgnoreString :: Sqlite.Query
insertOrIgnoreString = "INSERT OR IGNORE INTO strings(value) VALUES (?);"

getIpeStrings :: Sqlite.Query
getIpeStrings =
  "\
  \ SELECT                                             \
  \   table_name.id AS table_name,                     \
  \   type_desc.id AS type_desc,                       \
  \   label.id AS label,                               \
  \   module_name.id AS module_name,                   \
  \   src_loc.id AS src_loc                            \
  \ FROM strings AS table_name    \
  \ JOIN strings AS type_desc    ON ? = type_desc.value   \
  \ JOIN strings AS label        ON ? = label.value       \
  \ JOIN strings AS module_name  ON ? = module_name.value \
  \ JOIN strings AS src_loc      ON ? = src_loc.value     \
  \  WHERE ? = table_name.value ;"

getStringEntry :: Sqlite.Query
getStringEntry = "SELECT id, value FROM strings WHERE value = ?;"
