module IpeDb.Query (
  withInfoProvDb,
  setupInfoProvDb,
  populateFromEventlog,
  lookupInfoProv,
  insertInfoProv,
  listInfoProvs,
) where

import qualified IpeDb.Eventlog.Index as Index
import IpeDb.InfoProv
import IpeDb.Types

-- ----------------------------------------------------------------------------
-- High Level API
-- ----------------------------------------------------------------------------

withInfoProvDb :: FilePath -> (InfoProvDb -> IO a) -> IO a
withInfoProvDb fp act =
  Index.withDatabase fp (\conn -> act InfoProvDb{conn})

setupInfoProvDb :: InfoProvDb -> IO ()
setupInfoProvDb db =
  Index.setupDb db.conn

populateFromEventlog :: InfoProvDb -> FilePath -> IO ()
populateFromEventlog db fp =
  Index.generateInfoProvDb db.conn fp

lookupInfoProv :: InfoProvDb -> IpeId -> IO (Maybe InfoProv)
lookupInfoProv db ipeId = do
  Index.findOneInfoProv db.conn ipeId

insertInfoProv :: InfoProvDb -> InfoProv -> IO ()
insertInfoProv db ipe = do
  Index.insertInfoProv db.conn ipe

listInfoProvs :: InfoProvDb -> IO [InfoProv]
listInfoProvs db = do
  Index.findAllInfoProvs db.conn
