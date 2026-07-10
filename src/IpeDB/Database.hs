{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : IpeDB.Database
Description : This modules provides a database instance backed by @lsm-tree@.
Stability   : experimental
Portability : portable
-}
module IpeDB.Database (
  -- * Constraints
  Key,
  Value,
  SerialiseVia (..),
  SerialiseViaBinary (..),

  -- * Sessions
  Session,
  SessionOptions (maybeSessionRoot),
  defaultSessionOptions,
  withNewSession,

  -- * Tables
  Table,
  withNewTable,
  inserts,
  lookups,
  insert,
  lookup,

  -- ** Table Options
  TableOptions (..),
  defaultTableOptions,

  -- ** Table Enumerating
  IteratorOptions (iteratorBufferSize),
  defaultIteratorOptions,
  withIterator,

  -- ** Table Indexing
  IndexerOptions (indexerBufferSize),
  defaultIndexerOptions,
  indexer,

  -- ** Table Serialisation
  TableFormat (..),
  defaultTableFormat,
  withTableFrom,
  saveTable,
) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Check qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Exception (Exception, SomeException (..), bracketOnError, bracket_, catch, throwIO)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Binary (Binary)
import Data.Binary qualified as B
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce (Coercible, coerce)
import Data.Default (Default (..))
import Data.Machine ((~>))
import Data.Machine qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Void (Void)
import Data.Word (Word32)
import Database.LSMTree qualified as LSMT
import System.Directory qualified as SD
import System.FS.API.Strict qualified as FS
import System.FS.BlockIO.IO qualified as BIO
import System.FilePath qualified as SF
import System.IO.Temp (withSystemTempDirectory, withTempDirectory)
import Prelude hiding (lookup)

--------------------------------------------------------------------------------
-- Constraints
--------------------------------------------------------------------------------

type Key k = LSMT.SerialiseKey k

type Value v = (LSMT.SerialiseValue v, LSMT.ResolveValue v)

--------------------------------------------------------------------------------
-- Deriving Via Newtype

{- |
Wrapper that derives the constraints required by the database backend via `Coercible`.
-}
newtype SerialiseVia v u = SerialiseVia {value :: v}

--------------------------------------------------------------------------------
-- Key Via Newtype

instance (Coercible v u, LSMT.SerialiseKey u) => LSMT.SerialiseKey (SerialiseVia v u) where
  serialiseKey :: SerialiseVia v u -> LSMT.RawBytes
  serialiseKey = LSMT.serialiseKey . coerce @v @u . (.value)

  deserialiseKey :: LSMT.RawBytes -> SerialiseVia v u
  deserialiseKey = SerialiseVia . coerce @u @v . LSMT.deserialiseKey

--------------------------------------------------------------------------------
-- Value Via Newtype

instance (Coercible v u, LSMT.SerialiseValue u) => LSMT.SerialiseValue (SerialiseVia v u) where
  serialiseValue :: SerialiseVia v u -> LSMT.RawBytes
  serialiseValue = LSMT.serialiseValue . coerce @_ @u

  deserialiseValue :: LSMT.RawBytes -> SerialiseVia v u
  deserialiseValue = coerce @u @_ . LSMT.deserialiseValue

deriving via LSMT.ResolveAsFirst (SerialiseVia v u) instance LSMT.ResolveValue (SerialiseVia v u)

--------------------------------------------------------------------------------
-- Deriving Via Binary

{- |
Wrapper that derives the constraints required by the database backend via a `Binary` instance.
-}
newtype SerialiseViaBinary v = SerialiseViaBinary {value :: v}

--------------------------------------------------------------------------------
-- Key Via Binary

-- instance (Binary v) => Key (SerialiseViaBinary v)

instance (Binary v) => LSMT.SerialiseKey (SerialiseViaBinary v) where
  serialiseKey :: SerialiseViaBinary v -> LSMT.RawBytes
  serialiseKey = LSMT.serialiseKey . B.encode . (.value)

  deserialiseKey :: LSMT.RawBytes -> SerialiseViaBinary v
  deserialiseKey = SerialiseViaBinary . B.decode . LSMT.deserialiseKey

--------------------------------------------------------------------------------
-- Value Via Binary

instance (Binary v) => LSMT.SerialiseValue (SerialiseViaBinary v) where
  serialiseValue :: SerialiseViaBinary v -> LSMT.RawBytes
  serialiseValue = LSMT.serialiseValue . B.encode . (.value)

  deserialiseValue :: LSMT.RawBytes -> SerialiseViaBinary v
  deserialiseValue = SerialiseViaBinary . B.decode . LSMT.deserialiseValue

deriving via LSMT.ResolveAsFirst (SerialiseViaBinary v) instance LSMT.ResolveValue (SerialiseViaBinary v)

--------------------------------------------------------------------------------
-- Sessions
--------------------------------------------------------------------------------

{- |
Representation of database sessions.
-}
data Session
  = Session
  { mountPoint :: FS.MountPoint
  , sessionRoot :: FS.FsPath
  , session :: LSMT.Session IO
  }

{- |
The options for database sessions.
-}
newtype SessionOptions
  = SessionOptions
  { maybeSessionRoot :: Maybe FilePath
  }

{- |
The default database session options.
-}
defaultSessionOptions :: SessionOptions
defaultSessionOptions =
  SessionOptions
    { maybeSessionRoot = Nothing
    }

instance Default SessionOptions where
  def :: SessionOptions
  def = defaultSessionOptions

{- |
Run an action with a new session.
-}
withNewSession ::
  SessionOptions ->
  (Session -> IO r) ->
  IO r
withNewSession SessionOptions{..} action = do
  -- Create a temporary directory for the database session.
  let withSessionDir :: (FilePath -> IO a) -> IO a
      withSessionDir = case maybeSessionRoot of
        Nothing -> withSystemTempDirectory "eventlog-live"
        Just sessionRoot -> withTempDirectory sessionRoot "eventlog-live"
  withSessionDir $ \sessionRoot -> do
    -- Create the LSM Tree session.
    !sessionAbsRoot <- SD.makeAbsolute sessionRoot
    let (!mountPointPath, !sessionRelRoot) = SF.splitDrive sessionAbsRoot
    let !mountPoint = FS.MountPoint mountPointPath
    let !sessionRelRootDirs = SF.splitDirectories sessionRelRoot
    let !sessionRootFsPath = FS.mkFsPath sessionRelRootDirs
    let !sessionDirFsPath = sessionRootFsPath FS.</> FS.mkFsPath ["session"]
    BIO.withIOHasBlockIO mountPoint BIO.defaultIOCtxParams $ \hasFS hasBlockIO -> do
      -- Create the session directory.
      FS.createDirectoryIfMissing hasFS True sessionDirFsPath
      -- Create the LSM Tree session.
      let sessionSalt = 0
      LSMT.withNewSession mempty hasFS hasBlockIO sessionSalt sessionDirFsPath $ \session ->
        -- Run the action with the session.
        action Session{sessionRoot = sessionRootFsPath, ..}

--------------------------------------------------------------------------------
-- Tables
--------------------------------------------------------------------------------

{- |
A table.

Use `withNewTable`, `withNewTable`, or `withTableFrom` to construct a table.
-}
data Table k v
  = Table
  { session :: !Session
  , table :: !(LSMT.Table IO k v Void)
  }

{- |
Configuration options for tables, which can be used for performance tuning.
-}
newtype TableOptions
  = -- | See the @lsm-tree@ documentation for `LSMT.TableConfig`.
    TableOptions
    { tableConfig :: LSMT.TableConfig
    }

{- |
The default `TableOptions`.
-}
defaultTableOptions :: TableOptions
defaultTableOptions =
  TableOptions
    { tableConfig = LSMT.defaultTableConfig
    }

instance Default TableOptions where
  def :: TableOptions
  def = defaultTableOptions

{- |
Run an action with a new table.
-}
withNewTable ::
  forall k v a.
  Session ->
  TableOptions ->
  (Table k v -> IO a) ->
  IO a
withNewTable session (TableOptions config) action = do
  -- Create a new LSM Tree table.
  LSMT.withTableWith config session.session $ \table ->
    -- Run the action.
    action Table{..}

{- |
Insert entries into a table.
-}
inserts ::
  (Key k, Value v) =>
  Table k v -> Vector (k, v) -> IO ()
inserts Table{..} =
  LSMT.inserts table . fmap (\(k, v) -> (k, v, Nothing))

{- |
Lookup entries from a table.
-}
lookups ::
  (Key k, Value v) =>
  Table k v -> Vector k -> IO (Vector (Maybe v))
lookups Table{..} =
  fmap (fmap LSMT.getValue) . LSMT.lookups table

{- |
Insert entries into a table.
-}
insert ::
  (Key k, Value v) =>
  Table k v -> k -> v -> IO ()
insert Table{..} k v =
  LSMT.insert table k v Nothing

{- |
Lookup one entry from a table.
-}
lookup ::
  (Key k, Value v) =>
  Table k v ->
  k ->
  IO (Maybe v)
lookup Table{..} =
  fmap LSMT.getValue . LSMT.lookup table

--------------------------------------------------------------------------------
-- Enumerating

{- |
The options for `withIterator`.

[`iteratorBufferSize` :: `Word32`]:
  The size of the elems buffer in number of elements.
  The default is 10.
-}
newtype IteratorOptions = IteratorOptions
  { iteratorBufferSize :: Word32
  }

{- |
The default options for `withIterator`.
-}
defaultIteratorOptions :: IteratorOptions
defaultIteratorOptions =
  IteratorOptions{iteratorBufferSize = 10}

instance Default IteratorOptions where
  def :: IteratorOptions
  def = defaultIteratorOptions

{- |
Stream entries from a table.
-}
withIterator ::
  (Key k, Value v) =>
  IteratorOptions ->
  Table k v ->
  (M.SourceT IO (k, v) -> IO a) ->
  IO a
withIterator options Table{..} action = do
  LSMT.withCursor table $ \cursor ->
    action $ M.MachineT $ do
      -- The plan for a source that repeatedly queries the cursor.
      let entrySourcePlan = do
            let n = fromIntegral options.iteratorBufferSize
            xs <- liftIO (LSMT.take n cursor)
            M.yield xs
            unless (V.length xs < n) entrySourcePlan

      -- Helper to convert LSMT.Entry to a key-value pair.
      let toKeyValue e = (LSMT.getEntryKey e, LSMT.getEntryValue e)

      M.runMachineT $
        M.construct entrySourcePlan ~> M.asParts ~> M.mapping toKeyValue

--------------------------------------------------------------------------------
-- Indexing

{- |
The options for `indexer`.

[`indexerBufferSize` :: `Word32`]:
  The size of the index buffer in number of elements.
  The default is 10.
-}
newtype IndexerOptions = IndexerOptions
  { indexerBufferSize :: Word32
  }

{- |
The default options for `indexer`.
-}
defaultIndexerOptions :: IndexerOptions
defaultIndexerOptions =
  IndexerOptions{indexerBufferSize = 10}

instance Default IndexerOptions where
  def :: IndexerOptions
  def = defaultIndexerOptions

{- |
Index data from a GHC event stream.
-}
indexer ::
  (Key k, Value v) =>
  (e -> Maybe (k, v)) ->
  IndexerOptions ->
  Table k v ->
  M.ProcessT IO e Void
indexer extractKV options table =
  M.mapping extractKV
    ~> M.asParts
    ~> M.buffered (fromIntegral options.indexerBufferSize)
    ~> M.mapping V.fromList
    ~> M.repeatedly (M.await >>= liftIO . inserts table)

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

{- |
The table serialisation formats.
-}
data TableFormat
  = {- |
    This format creates a /directory/ that contains an @lsm-tree@ snapshot.

    This format is the fastest to write, since it hard links to the runtime
    database files, but requires that the session root and the target path
    are on the same filesystem.
    -}
    LSMTreeSnapshotV2
  | {- |
    This format creates a tar archive of an `LSMTreeSnapshotV2` export.

    This requires copying the database files.
    -}
    LSMTreeSnapshotV2Tar
  | {- |
    This format creates a compressed `LSMTreeSnapshotV2Tar` export.

    This format is the slowest to write, since it requires compressing the
    database files, but the it results in significantly smaller files.
    -}
    LSMTreeSnapshotV2TarGz

{- |
The default table format.
-}
defaultTableFormat :: TableFormat
defaultTableFormat = LSMTreeSnapshotV2TarGz

instance Default TableFormat where
  def :: TableFormat
  def = LSMTreeSnapshotV2TarGz

{- |
Run an action with a table.
-}
withTableFrom ::
  forall k v a.
  (LSMT.ResolveValue v) =>
  Session ->
  FilePath ->
  TableFormat ->
  (Table k v -> IO a) ->
  IO a
withTableFrom session inputRelPath inputFormat action = do
  -- Find the absolute file path to the table.
  inputAbsPath <- SD.makeAbsolute inputRelPath

  -- Load a table in the LSMTreeSnapshotV2 format.
  let loadLSMTreeSnapshotV2 :: IO ()
      loadLSMTreeSnapshotV2 = do
        -- Try to represent the target directory as an FsPath.
        let FS.MountPoint mountPointPath = session.mountPoint
        let !targetFsPath =
              fromMaybe (error $ "Cannot hardlink from " <> inputAbsPath <> "; not under mount point " <> mountPointPath <> ".") $
                FS.fsFromFilePath session.mountPoint inputAbsPath
        -- Export the snapshot.
        let snapshotFsPath = targetFsPath FS.</> FS.mkFsPath [snapshotRelPath]
        LSMT.importSnapshot session.session snapshotName snapshotFsPath

  -- Load a table in the LSMTreeSnapshotV2Tar format.
  let loadLSMTreeSnapshotV2Tar :: (BSL.ByteString -> BSL.ByteString) -> IO ()
      loadLSMTreeSnapshotV2Tar decompress = do
        -- Create temporary @active-import@ directory in the database session root.
        let !sessionRootPath = FS.fsToFilePath session.mountPoint session.sessionRoot
        withTempDirectory sessionRootPath "active-import" $ \importDir -> do
          -- Extract the snapshot to @active-import/$tableName@.
          !importAbsDir <- SD.makeAbsolute importDir
          let !importDirFsPath = fromJust (FS.fsFromFilePath session.mountPoint importAbsDir)
          let !snapshotDirFsPath = importDirFsPath FS.</> FS.mkFsPath [snapshotRelPath]
          tarByteString <- BSL.readFile inputAbsPath
          let tarEntries = Tar.read . decompress $ tarByteString
          let tarCheck entry = SomeException <$> Tar.checkEntrySecurity entry
          Tar.unpackAndCheck tarCheck importAbsDir tarEntries
          -- Import the snapshot from @active-import/$tableName@.
          LSMT.importSnapshot session.session snapshotName snapshotDirFsPath

  -- Load a table based on the inferred table format.
  let loadSnapshot :: IO ()
      loadSnapshot = case inputFormat of
        LSMTreeSnapshotV2 -> loadLSMTreeSnapshotV2
        LSMTreeSnapshotV2Tar -> loadLSMTreeSnapshotV2Tar id
        LSMTreeSnapshotV2TarGz -> loadLSMTreeSnapshotV2Tar GZip.decompress

  -- Load the snapshot.
  bracketOnError loadSnapshot (const $ deleteSnapshot session.session) $ \() ->
    -- Open the table from the snapshot.
    LSMT.withTableFromSnapshot session.session snapshotName snapshotLabel $ \table -> do
      -- Delete the snapshot.
      deleteSnapshot session.session
      -- Run the action.
      action Table{..}

{- |
Save a table.

The target path must not already exist.
-}
saveTable ::
  Table k v ->
  FilePath ->
  TableFormat ->
  IO ()
saveTable table targetRelPath targetFormat = do
  -- If the target path already exists, throw a TargetExistsError.
  targetExists <- SD.doesPathExist targetRelPath
  when targetExists . throwIO $ TargetExistsError targetRelPath

  -- Export the lsm-tree snapshot by hard linking.
  let saveLSMTreeSnapshotV2 :: IO ()
      saveLSMTreeSnapshotV2 = do
        -- Try to represent the target directory as an FsPath.
        let FS.MountPoint mountPointPath = table.session.mountPoint
        targetAbsPath <- SD.makeAbsolute targetRelPath
        let !targetFsPath =
              fromMaybe (error $ "Cannot hardlink to " <> targetRelPath <> "; not under mount point " <> mountPointPath <> ".") $
                FS.fsFromFilePath table.session.mountPoint targetAbsPath

        -- Manage the target directory.
        let createTarget = SD.createDirectory targetAbsPath
        let removeTarget = SD.removeDirectory targetAbsPath
        -- Export the snapshot.
        bracketOnError createTarget (const removeTarget) . const $ do
          let snapshotFsPath = targetFsPath FS.</> FS.mkFsPath [snapshotRelPath]
          LSMT.exportSnapshot table.session.session snapshotName snapshotFsPath

  -- Export the lsm-tree snapshot by compressing and archiving.
  let saveLSMTreeSnapshotV2Tar :: (BSL.ByteString -> BSL.ByteString) -> IO ()
      saveLSMTreeSnapshotV2Tar compress = do
        -- Create temporary @active-export@ directory in the database session root.
        let !sessionRootPath = FS.fsToFilePath table.session.mountPoint table.session.sessionRoot
        withTempDirectory sessionRootPath "active-export" $ \exportRootDir -> do
          -- Export the snapshot to the temporary @active-export@ directory.
          let !snapshotDir = exportRootDir SF.</> snapshotRelPath
          !snapshotAbsDir <- SD.makeAbsolute snapshotDir
          let !snapshotDirFsPath = fromJust (FS.fsFromFilePath table.session.mountPoint snapshotAbsDir)
          LSMT.exportSnapshot table.session.session snapshotName snapshotDirFsPath
          -- Create the output tar archive.
          BSL.writeFile targetRelPath . compress =<< Tar.write' =<< Tar.pack' exportRootDir [snapshotRelPath]

  bracket_ (saveSnapshot table.table) (deleteSnapshot table.session.session) $
    case targetFormat of
      LSMTreeSnapshotV2 -> saveLSMTreeSnapshotV2
      LSMTreeSnapshotV2Tar -> saveLSMTreeSnapshotV2Tar id
      LSMTreeSnapshotV2TarGz -> saveLSMTreeSnapshotV2Tar GZip.compress

--------------------------------------------------------------------------------
-- Internal Helpers

{- |
Internal helper.

Save an LSM Tree snapshot.
-}
saveSnapshot :: LSMT.Table IO k v Void -> IO ()
saveSnapshot = LSMT.saveSnapshot snapshotName snapshotLabel

{- |
Internal helper.

Delete an LSM Tree snapshot, but ignore any `LSMT.ErrSnapshotDoesNotExist` errors.
-}
deleteSnapshot :: LSMT.Session IO -> IO ()
deleteSnapshot session =
  LSMT.deleteSnapshot session snapshotName
    `catch` \LSMT.ErrSnapshotDoesNotExist{} -> pure ()

{- |
Internal helper.

The relative `FilePath` for the snapshot directory used by all databases.
-}
snapshotRelPath :: FilePath
snapshotRelPath = "data"

{- |
Internal helper.

The `LSMT.SnapshotName` used by all databases.
-}
snapshotName :: LSMT.SnapshotName
snapshotName = LSMT.toSnapshotName snapshotRelPath

{- |
Internal helper.

The `LSMT.SnapshotLabel` used by all databases.
-}
snapshotLabel :: LSMT.SnapshotLabel
snapshotLabel = LSMT.SnapshotLabel (T.pack "ipedb:1")

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

{- |
This error is raised if the target path already exists.
-}
newtype TargetExistsError = TargetExistsError FilePath
  deriving (Show)

instance Exception TargetExistsError
