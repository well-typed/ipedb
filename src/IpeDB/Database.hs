{- |
Module      : IpeDB.Database
Description : This modules provides a generic abstraction over @lsm-tree@ databases.
Stability   : experimental
Portability : portable
-}
module IpeDB.Database (
  -- * Backends
  Backend (..),

  -- * Sessions
  Session,
  SessionOptions (maybeSessionRoot),
  defaultLSMTreeSessionOptions,
  withNewSession,

  -- * Tables
  Table,
  TableSpec (..),
  withNewTable,
  withNewTableWith,
  inserts,
  lookup,
  lookups,

  -- ** Table Options
  TableConfig (..),
  defaultLSMTreeTableConfig,

  -- ** Table Enumerating
  IteratorOptions (iteratorBufferSize),
  defaultIteratorOptions,
  withIterator,

  -- ** Table Indexing
  IndexerOptions (indexerBufferSize),
  defaultIndexerOptions,
  indexer,

  -- ** Table Export/Import
  withTableFrom,
  saveTable,
  TableFormat (..),
  defaultTableFormat,

  -- * Serialisation
  SerialiseViaBinary (..),
  SerialiseVia (..),

  -- * Errors
  InvalidTableNameError (..),
  TargetExistsError (..),
) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Check qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Exception (Exception (..), SomeException (..), bracketOnError, bracket_, catch, throwIO)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Binary (Binary)
import Data.Binary qualified as B
import Data.ByteString.Lazy qualified as BSL
import Data.Coerce (Coercible, coerce)
import Data.Default (Default (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import Data.Machine ((~>))
import Data.Machine qualified as M
import Data.Maybe (fromJust, fromMaybe)
import Data.String (IsString (..))
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
-- Backends
--------------------------------------------------------------------------------

type data Backend = InMemory | LSMTree

--------------------------------------------------------------------------------
-- Sessions
--------------------------------------------------------------------------------

{- |
Representation of database sessions.
-}
data Session (b :: Backend) where
  InMemorySession ::
    Session InMemory
  LSMTreeSession ::
    { lsmTreeMountPoint :: FS.MountPoint
    , lsmTreeSessionRoot :: FS.FsPath
    , lsmTreeSession :: LSMT.Session IO
    } ->
    Session LSMTree

{- |
The options for database sessions.
-}
data SessionOptions (b :: Backend) where
  IntMapSessionOptions ::
    SessionOptions InMemory
  LSMTreeSessionOptions ::
    { maybeSessionRoot :: Maybe FilePath
    } ->
    SessionOptions LSMTree

{- |
The default in-memory session options.
-}
defaultIntMapSessionOptions :: SessionOptions InMemory
defaultIntMapSessionOptions =
  IntMapSessionOptions
    {
    }

{- |
The default database session options.
-}
defaultLSMTreeSessionOptions :: SessionOptions LSMTree
defaultLSMTreeSessionOptions =
  LSMTreeSessionOptions
    { maybeSessionRoot = Nothing
    }

instance Default (SessionOptions InMemory) where
  def :: SessionOptions InMemory
  def = defaultIntMapSessionOptions

instance Default (SessionOptions LSMTree) where
  def :: SessionOptions LSMTree
  def = defaultLSMTreeSessionOptions

{- |
Run an action with a new session.
-}
withNewSession ::
  SessionOptions b ->
  (Session b -> IO r) ->
  IO r
withNewSession IntMapSessionOptions{} action = do
  action InMemorySession{}
withNewSession LSMTreeSessionOptions{..} action = do
  -- Create a temporary directory for the database session.
  let withSessionDir :: (FilePath -> IO a) -> IO a
      withSessionDir = case maybeSessionRoot of
        Nothing -> withSystemTempDirectory "eventlog-live"
        Just sessionRoot -> withTempDirectory sessionRoot "eventlog-live"
  withSessionDir $ \sessionRoot -> do
    -- Create the LSM Tree session.
    !sessionAbsRoot <- SD.makeAbsolute sessionRoot
    let (!mountPointPath, !sessionRelRoot) = SF.splitDrive sessionAbsRoot
    let !lsmTreeMountPoint = FS.MountPoint mountPointPath
    let !sessionRelRootDirs = SF.splitDirectories sessionRelRoot
    let !sessionRootFsPath = FS.mkFsPath sessionRelRootDirs
    let !sessionDirFsPath = sessionRootFsPath FS.</> FS.mkFsPath ["session"]
    BIO.withIOHasBlockIO lsmTreeMountPoint BIO.defaultIOCtxParams $ \hasFS hasBlockIO -> do
      -- Create the session directory.
      FS.createDirectoryIfMissing hasFS True sessionDirFsPath
      -- Create the LSM Tree session.
      let sessionSalt = 0
      LSMT.withNewSession mempty hasFS hasBlockIO sessionSalt sessionDirFsPath $ \lsmTreeSession ->
        -- Run the action with the session.
        action LSMTreeSession{lsmTreeSessionRoot = sessionRootFsPath, ..}

--------------------------------------------------------------------------------
-- Tables
--------------------------------------------------------------------------------

{- |
A table.

Use `withNewTable`, `withNewTableWith`, or `withTableFrom` to construct a table.
-}
data Table (b :: Backend) (k :: Type) (v :: Type) where
  InMemoryTable ::
    { inMemorySpec :: !(TableSpec InMemory k v)
    , inMemoryTable :: !(IORef (HashMap k v))
    } ->
    Table InMemory k v
  LSMTreeTable ::
    { lsmTreeSpec :: !(TableSpec LSMTree k v)
    , lsmTreeSession :: !(Session LSMTree)
    , lsmTreeTable :: !(LSMT.Table IO k v Void)
    } ->
    Table LSMTree k v

{- |
A table specification.

This contains the data needed to interact with the database backend.

For @lsm-tree@, this contains instances for the serialisation classes for the
key and value types, a table name, and a label that describes the types of the
table entries.

The "IpeDB.Types.CostCentre" and "IpeDB.Types.InfoProv" modules define default
table specifications for their respective databases.
-}
data TableSpec (b :: Backend) (k :: Type) (v :: Type) where
  InMemoryTableSpec ::
    (Hashable k) =>
    TableSpec InMemory k v
  LSMTreeTableSpec ::
    (LSMT.SerialiseKey k, LSMT.SerialiseValue v, LSMT.ResolveValue v) =>
    { lsmTreeTableName :: String
    , lsmTreeTableLabel :: String
    } ->
    TableSpec LSMTree k v

{- |
The table serialisation formats.
-}
data TableFormat (b :: Backend) where
  {- |
    This format creates a /directory/ that contains an @lsm-tree@ snapshot.

    This format is the fastest to write, since it hard links to the runtime
    database files, but requires that the session root and the target path
    are on the same filesystem.
  -}
  LSMTreeSnapshotV2 :: TableFormat LSMTree
  {- |
    This format creates a tar archive of an `LSMTreeSnapshotV2` export.

    This requires copying the database files.
  -}
  LSMTreeSnapshotV2Tar :: TableFormat LSMTree
  {- |
    This format creates a compressed `LSMTreeSnapshotV2Tar` export.

    This format is the slowest to write, since it requires compressing the
    database files, but the it results in significantly smaller files.
  -}
  LSMTreeSnapshotV2TarGz :: TableFormat LSMTree

{- |
The default table format.
-}
defaultTableFormat :: TableFormat LSMTree
defaultTableFormat = LSMTreeSnapshotV2TarGz

instance Default (TableFormat LSMTree) where
  def :: TableFormat LSMTree
  def = LSMTreeSnapshotV2TarGz

{- |
Configuration options for tables, which can be used for performance tuning.
-}
data TableConfig (b :: Backend) where
  InMemoryTableConfig ::
    TableConfig InMemory
  LSMTreeTableConfig ::
    LSMT.TableConfig ->
    TableConfig LSMTree

instance Default (TableConfig InMemory) where
  def :: TableConfig InMemory
  def = InMemoryTableConfig

{- |
The default `TableConfig`.
-}
defaultLSMTreeTableConfig :: TableConfig LSMTree
defaultLSMTreeTableConfig =
  LSMTreeTableConfig LSMT.defaultTableConfig

instance Default (TableConfig LSMTree) where
  def :: TableConfig LSMTree
  def = defaultLSMTreeTableConfig

{- |
Run an action with a new table.
-}
withNewTable ::
  forall b k v a.
  Session b ->
  TableSpec b k v ->
  (Table b k v -> IO a) ->
  IO a
withNewTable session = \case
  spec@InMemoryTableSpec{} ->
    withNewTableWith session spec def
  spec@LSMTreeTableSpec{} ->
    withNewTableWith session spec def

{- |
Run an action with a new table.
-}
withNewTableWith ::
  forall b k v a.
  Session b ->
  TableSpec b k v ->
  TableConfig b ->
  (Table b k v -> IO a) ->
  IO a
withNewTableWith _session inMemorySpec@InMemoryTableSpec{} InMemoryTableConfig{} action = do
  inMemoryTable <- newIORef HashMap.empty
  action InMemoryTable{..}
withNewTableWith lsmTreeSession lsmTreeSpec (LSMTreeTableConfig config) action = do
  -- Check if the table name is a valid snapshot name.
  unless (LSMT.isValidSnapshotName lsmTreeSpec.lsmTreeTableName) $
    throwIO $
      InvalidTableNameError lsmTreeSpec.lsmTreeTableName

  -- Create a new LSM Tree table.
  LSMT.withTableWith config lsmTreeSession.lsmTreeSession $ \lsmTreeTable ->
    -- Run the action.
    action LSMTreeTable{..}

{- |
Run an action with a table.
-}
withTableFrom ::
  forall b k v a.
  Session b ->
  TableSpec b k v ->
  FilePath ->
  TableFormat b ->
  (Table b k v -> IO a) ->
  IO a
withTableFrom lsmTreeSession lsmTreeSpec@LSMTreeTableSpec{..} inputRelPath inputFormat action = do
  -- Find the absolute file path to the table.
  inputAbsPath <- SD.makeAbsolute inputRelPath

  -- Snapshot name and label for the lsm-tree library.
  let !snapshotName = LSMT.toSnapshotName lsmTreeTableName
  let !snapshotLabel = LSMT.SnapshotLabel (fromString lsmTreeTableLabel)

  -- Load a table in the LSMTreeSnapshotV2 format.
  let loadLSMTreeSnapshotV2 :: IO ()
      loadLSMTreeSnapshotV2 = do
        -- Try to represent the target directory as an FsPath.
        let FS.MountPoint mountPointPath = lsmTreeSession.lsmTreeMountPoint
        let !targetFsPath =
              fromMaybe (error $ "Cannot hardlink from " <> inputAbsPath <> "; not under mount point " <> mountPointPath <> ".") $
                FS.fsFromFilePath lsmTreeSession.lsmTreeMountPoint inputAbsPath
        -- Export the snapshot.
        let snapshotFsPath = targetFsPath FS.</> FS.mkFsPath [lsmTreeTableName]
        LSMT.importSnapshot lsmTreeSession.lsmTreeSession snapshotName snapshotFsPath

  -- Load a table in the LSMTreeSnapshotV2Tar format.
  let loadLSMTreeSnapshotV2Tar :: (BSL.ByteString -> BSL.ByteString) -> IO ()
      loadLSMTreeSnapshotV2Tar decompress = do
        -- Create temporary @active-import@ directory in the database session root.
        let !sessionRootPath = FS.fsToFilePath lsmTreeSession.lsmTreeMountPoint lsmTreeSession.lsmTreeSessionRoot
        withTempDirectory sessionRootPath "active-import" $ \importDir -> do
          -- Extract the snapshot to @active-import/$tableName@.
          !importAbsDir <- SD.makeAbsolute importDir
          let !importDirFsPath = fromJust (FS.fsFromFilePath lsmTreeSession.lsmTreeMountPoint importAbsDir)
          let !snapshotDirFsPath = importDirFsPath FS.</> FS.mkFsPath [lsmTreeTableName]
          tarByteString <- BSL.readFile inputAbsPath
          let tarEntries = Tar.read . decompress $ tarByteString
          let tarCheck entry = SomeException <$> Tar.checkEntrySecurity entry
          Tar.unpackAndCheck tarCheck importAbsDir tarEntries
          -- Import the snapshot from @active-import/$tableName@.
          LSMT.importSnapshot lsmTreeSession.lsmTreeSession snapshotName snapshotDirFsPath

  -- Load a table based on the inferred table format.
  let loadSnapshot :: IO ()
      loadSnapshot = case inputFormat of
        LSMTreeSnapshotV2 -> loadLSMTreeSnapshotV2
        LSMTreeSnapshotV2Tar -> loadLSMTreeSnapshotV2Tar id
        LSMTreeSnapshotV2TarGz -> loadLSMTreeSnapshotV2Tar GZip.decompress

  -- Load the snapshot.
  bracketOnError loadSnapshot (\() -> deleteLSMTreeSnapshot lsmTreeSession.lsmTreeSession snapshotName) $ \() ->
    -- Open the table from the snapshot.
    LSMT.withTableFromSnapshot lsmTreeSession.lsmTreeSession snapshotName snapshotLabel $ \lsmTreeTable -> do
      -- Delete the snapshot.
      deleteLSMTreeSnapshot lsmTreeSession.lsmTreeSession snapshotName
      -- Run the action.
      action LSMTreeTable{..}
withTableFrom _ _ _ _ _ = undefined

{- |
Save a table.

The target path must not already exist.
-}
saveTable :: Table b k v -> FilePath -> TableFormat b -> IO ()
saveTable InMemoryTable{} _ targetFormat =
  case targetFormat of {}
saveTable LSMTreeTable{lsmTreeSpec = LSMTreeTableSpec{..}, lsmTreeSession = LSMTreeSession{..}, ..} targetRelPath targetFormat = do
  -- If the target path already exists, throw a TargetExistsError.
  targetExists <- SD.doesPathExist targetRelPath
  when targetExists . throwIO $ TargetExistsError targetRelPath

  -- Save an lsm-tree snapshot.
  let !snapshotName = LSMT.toSnapshotName lsmTreeTableName
  let !snapshotLabel = LSMT.SnapshotLabel (fromString lsmTreeTableLabel)
  let !saveSnapshot = LSMT.saveSnapshot snapshotName snapshotLabel lsmTreeTable

  -- Export the lsm-tree snapshot by hard linking.
  let saveLSMTreeSnapshotV2 :: IO ()
      saveLSMTreeSnapshotV2 = do
        -- Try to represent the target directory as an FsPath.
        let FS.MountPoint mountPointPath = lsmTreeMountPoint
        targetAbsPath <- SD.makeAbsolute targetRelPath
        let !targetFsPath =
              fromMaybe (error $ "Cannot hardlink to " <> targetRelPath <> "; not under mount point " <> mountPointPath <> ".") $
                FS.fsFromFilePath lsmTreeMountPoint targetAbsPath

        -- Manage the target directory.
        let createTarget = SD.createDirectory targetAbsPath
        let removeTarget = SD.removeDirectory targetAbsPath
        -- Export the snapshot.
        bracketOnError createTarget (const removeTarget) . const $ do
          let snapshotFsPath = targetFsPath FS.</> FS.mkFsPath [lsmTreeTableName]
          LSMT.exportSnapshot lsmTreeSession snapshotName snapshotFsPath

  -- Export the lsm-tree snapshot by compressing and archiving.
  let saveLSMTreeSnapshotV2Tar :: (BSL.ByteString -> BSL.ByteString) -> IO ()
      saveLSMTreeSnapshotV2Tar compress = do
        -- Create temporary @active-export@ directory in the database session root.
        let !sessionRootPath = FS.fsToFilePath lsmTreeMountPoint lsmTreeSessionRoot
        withTempDirectory sessionRootPath "active-export" $ \exportRootDir -> do
          -- Export the snapshot to the temporary @active-export@ directory.
          let !snapshotDir = exportRootDir SF.</> lsmTreeTableName
          !snapshotAbsDir <- SD.makeAbsolute snapshotDir
          let !snapshotDirFsPath = fromJust (FS.fsFromFilePath lsmTreeMountPoint snapshotAbsDir)
          LSMT.exportSnapshot lsmTreeSession snapshotName snapshotDirFsPath
          -- Create the output tar archive.
          BSL.writeFile targetRelPath . compress =<< Tar.write' =<< Tar.pack' exportRootDir [lsmTreeTableName]

  bracket_ saveSnapshot (deleteLSMTreeSnapshot lsmTreeSession snapshotName) $
    case targetFormat of
      LSMTreeSnapshotV2 -> saveLSMTreeSnapshotV2
      LSMTreeSnapshotV2Tar -> saveLSMTreeSnapshotV2Tar id
      LSMTreeSnapshotV2TarGz -> saveLSMTreeSnapshotV2Tar GZip.compress

{- |
Insert entries into a table.
-}
inserts :: Table b k v -> Vector (k, v) -> IO ()
inserts = \case
  InMemoryTable{inMemorySpec = InMemoryTableSpec{}, ..} -> \kvs ->
    modifyIORef' inMemoryTable (\t -> foldr (uncurry HashMap.insert) t kvs)
  LSMTreeTable{lsmTreeSpec = LSMTreeTableSpec{}, ..} ->
    LSMT.inserts lsmTreeTable . fmap (\(k, v) -> (k, v, Nothing))

{- |
Lookup one entry from a table.
-}
lookup :: Table b k v -> k -> IO (Maybe v)
lookup = \case
  InMemoryTable{inMemorySpec = InMemoryTableSpec{}, ..} -> \k -> do
    t <- readIORef inMemoryTable
    pure $ HashMap.lookup k t
  LSMTreeTable{lsmTreeSpec = LSMTreeTableSpec{}, ..} ->
    fmap LSMT.getValue . LSMT.lookup lsmTreeTable

{- |
Lookup entries from a table.
-}
lookups :: Table b k v -> Vector k -> IO (Vector (Maybe v))
lookups = \case
  InMemoryTable{inMemorySpec = InMemoryTableSpec{}, ..} -> \ks -> do
    t <- readIORef inMemoryTable
    pure $ flip HashMap.lookup t <$> ks
  LSMTreeTable{lsmTreeSpec = LSMTreeTableSpec{}, ..} ->
    fmap (fmap LSMT.getValue) . LSMT.lookups lsmTreeTable

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
  IteratorOptions ->
  Table b k v ->
  (M.SourceT IO (k, v) -> IO a) ->
  IO a
withIterator IteratorOptions{} InMemoryTable{inMemorySpec = InMemoryTableSpec{}, ..} action = do
  t <- readIORef inMemoryTable
  action $ M.source (HashMap.toList t)
withIterator IteratorOptions{..} LSMTreeTable{lsmTreeSpec = LSMTreeTableSpec{}, ..} action = do
  LSMT.withCursor lsmTreeTable $ \cursor ->
    action $ M.MachineT $ do
      -- The plan for a source that repeatedly queries the cursor.
      let entrySourcePlan = do
            let n = fromIntegral iteratorBufferSize
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
data IndexerOptions e k v = IndexerOptions
  { extractKV :: !(e -> Maybe (k, v))
  , indexerBufferSize :: !Word32
  }

{- |
The default options for `indexer`.
-}
defaultIndexerOptions :: (e -> Maybe (k, v)) -> IndexerOptions e k v
defaultIndexerOptions extractKV =
  IndexerOptions{indexerBufferSize = 10, ..}

{- |
Index data from a GHC event stream.
-}
indexer ::
  IndexerOptions e k v ->
  Table b k v ->
  M.ProcessT IO e Void
indexer options table =
  M.mapping options.extractKV
    ~> M.asParts
    ~> M.buffered (fromIntegral options.indexerBufferSize)
    ~> M.mapping V.fromList
    ~> M.repeatedly (M.await >>= liftIO . inserts table)

--------------------------------------------------------------------------------
-- Helpers

{- |
Internal helper.

Delete an LSM Tree snapshot, but ignore any `LSMT.ErrSnapshotDoesNotExist` errors.
-}
deleteLSMTreeSnapshot :: LSMT.Session IO -> LSMT.SnapshotName -> IO ()
deleteLSMTreeSnapshot session snapshotName =
  LSMT.deleteSnapshot session snapshotName
    `catch` \LSMT.ErrSnapshotDoesNotExist{} -> pure ()

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

{- |
Wrapper that derives the required `LSMT.SerialiseKey` and `LSMT.SerialiseValue`
instances from a `Binary` instance.

Derives `LSMT.ResolveValue` via `LSMT.ResolveAsFirst`.
-}
newtype SerialiseViaBinary v = SerialiseViaBinary {value :: v}

instance (Binary v) => LSMT.SerialiseKey (SerialiseViaBinary v) where
  serialiseKey :: SerialiseViaBinary v -> LSMT.RawBytes
  serialiseKey = LSMT.serialiseKey . B.encode . (.value)

  deserialiseKey :: LSMT.RawBytes -> SerialiseViaBinary v
  deserialiseKey = SerialiseViaBinary . B.decode . LSMT.deserialiseKey

instance (Binary v) => LSMT.SerialiseValue (SerialiseViaBinary v) where
  serialiseValue :: SerialiseViaBinary v -> LSMT.RawBytes
  serialiseValue = LSMT.serialiseValue . B.encode . (.value)

  deserialiseValue :: LSMT.RawBytes -> SerialiseViaBinary v
  deserialiseValue = SerialiseViaBinary . B.decode . LSMT.deserialiseValue

deriving via LSMT.ResolveAsFirst (SerialiseViaBinary v) instance LSMT.ResolveValue (SerialiseViaBinary v)

{- |
Wrapper that derives the required `LSMT.SerialiseKey` and `LSMT.SerialiseValue`
instances by unwrapping the newtype.

Derives `LSMT.ResolveValue` via `LSMT.ResolveAsFirst`.
-}
newtype SerialiseVia v u = SerialiseVia {value :: v}

instance (Coercible v u, LSMT.SerialiseKey u) => LSMT.SerialiseKey (SerialiseVia v u) where
  serialiseKey :: SerialiseVia v u -> LSMT.RawBytes
  serialiseKey = LSMT.serialiseKey . coerce @_ @u

  deserialiseKey :: LSMT.RawBytes -> SerialiseVia v u
  deserialiseKey = coerce @u @_ . LSMT.deserialiseKey

instance (Coercible v u, LSMT.SerialiseValue u) => LSMT.SerialiseValue (SerialiseVia v u) where
  serialiseValue :: SerialiseVia v u -> LSMT.RawBytes
  serialiseValue = LSMT.serialiseValue . coerce @_ @u

  deserialiseValue :: LSMT.RawBytes -> SerialiseVia v u
  deserialiseValue = coerce @u @_ . LSMT.deserialiseValue

deriving via LSMT.ResolveAsFirst (SerialiseVia v u) instance LSMT.ResolveValue (SerialiseVia v u)

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

{- |
This error is raised if the table name is invalid.

See `LSMT.isValidSnapshotName`.
-}
newtype InvalidTableNameError = InvalidTableNameError String
  deriving (Show)

instance Exception InvalidTableNameError

{- |
This error is raised if the target path already exists.
-}
newtype TargetExistsError = TargetExistsError FilePath
  deriving (Show)

instance Exception TargetExistsError
