module IpeDb.Query (
  withInfoProvDb,
  lookupInfoProv,
) where

import qualified Database.SQLite.Simple as Sqlite
import Database.SQLite.Simple.Types (Only (..))
import IpeDb.InfoProv
import IpeDb.Table
import IpeDb.Types

-- ----------------------------------------------------------------------------
-- High Level API
-- ----------------------------------------------------------------------------

withInfoProvDb :: FilePath -> (InfoProvDb -> IO a) -> IO a
withInfoProvDb fp act = Sqlite.withConnection fp $ \conn ->
  act (InfoProvDb{conn})

lookupInfoProv :: InfoProvDb -> IpeId -> IO (Maybe InfoProv)
lookupInfoProv db ipeId = do
  r <- Sqlite.query db.conn findInfoTableQuery (Only ipeId)
  case r of
    [ipe] -> pure $ Just ipe
    _ -> pure $ Nothing
