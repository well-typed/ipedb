module IpeDb.Types
  ( InfoProvDb (..),
  )
where

import qualified Database.SQLite.Simple as Sqlite

-- ----------------------------------------------------------------------------
-- Db type
-- ----------------------------------------------------------------------------

data InfoProvDb = InfoProvDb
  { conn :: Sqlite.Connection
  }
