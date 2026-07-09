{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : IpeDB.Database.HashMap
Description : This modules provides a database instance backed by a `HashMap`.
Stability   : experimental
Portability : portable
-}
module IpeDB.Database.HashMap (
  -- * Constraints
  Key,
  Value,

  -- * Sessions
  Session,
  SessionOptions,
  defaultSessionOptions,
  withNewSession,

  -- * Tables
  Table,
  withNewTable,
  insert,
  lookup,

  -- ** Table Options
  TableOptions (..),
  defaultTableOptions,

  -- ** Table Enumerating
  IteratorOptions,
  defaultIteratorOptions,
  withIterator,

  -- ** Table Indexing
  IndexerOptions,
  defaultIndexerOptions,
  indexer,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Coerce (Coercible, coerce)
import Data.Default (Default (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable (..))
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Machine ((~>))
import Data.Machine qualified as M
import Data.Void (Void)
import IpeDB.Database.Class (SerialiseVia (..))
import Prelude hiding (lookup)

--------------------------------------------------------------------------------
-- Constraints
--------------------------------------------------------------------------------

type Key k = Hashable k

class Value v

instance Value v

--------------------------------------------------------------------------------
-- Key Via Newtype

instance (Coercible v u, Eq u) => Eq (SerialiseVia v u) where
  (==) :: SerialiseVia v u -> SerialiseVia v u -> Bool
  SerialiseVia v1 == SerialiseVia v2 = coerce @v @u v1 == coerce @v @u v2

instance (Coercible v u, Hashable u) => Hashable (SerialiseVia v u) where
  hashWithSalt :: Int -> SerialiseVia v u -> Int
  hashWithSalt salt (SerialiseVia v) = hashWithSalt salt (coerce @v @u v)

--------------------------------------------------------------------------------
-- Sessions
--------------------------------------------------------------------------------

{- |
Representation of database sessions.
-}
data Session = Session

{- |
The options for database sessions.
-}
data SessionOptions = SessionOptions

{- |
The default database session options.
-}
defaultSessionOptions :: SessionOptions
defaultSessionOptions = SessionOptions

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
withNewSession SessionOptions action =
  action Session

--------------------------------------------------------------------------------
-- Tables
--------------------------------------------------------------------------------

{- |
A table.

Use `withNewTable`, `withNewTable`, or `withTableFrom` to construct a table.
-}
newtype Table k v
  = Table
  { tableRef :: IORef (HashMap k v)
  }

{- |
Configuration options for tables, which can be used for performance tuning.
-}
data TableOptions = TableOptions

{- |
The default `TableOptions`.
-}
defaultTableOptions :: TableOptions
defaultTableOptions = TableOptions

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
withNewTable Session TableOptions action = do
  -- Create a new HashMap.
  tableRef <- IORef.newIORef HashMap.empty
  -- Run the action.
  action Table{..}

{- |
Insert entries into a table.
-}
insert ::
  (Key k) =>
  Table k v -> k -> v -> IO ()
insert Table{..} k v =
  IORef.modifyIORef' tableRef (HashMap.insert k v)

{- |
Lookup one entry from a table.
-}
lookup ::
  (Key k) =>
  Table k v ->
  k ->
  IO (Maybe v)
lookup Table{..} k =
  HashMap.lookup k <$> IORef.readIORef tableRef

--------------------------------------------------------------------------------
-- Enumerating

{- |
The options for `withIterator`.
-}
data IteratorOptions = IteratorOptions

{- |
The default options for `withIterator`.
-}
defaultIteratorOptions :: IteratorOptions
defaultIteratorOptions = IteratorOptions

instance Default IteratorOptions where
  def :: IteratorOptions
  def = defaultIteratorOptions

{- |
Stream entries from a table.
-}
withIterator ::
  forall k v a.
  IteratorOptions ->
  Table k v ->
  (M.SourceT IO (k, v) -> IO a) ->
  IO a
withIterator IteratorOptions Table{..} action = do
  table <- IORef.readIORef tableRef
  action $ M.source . HashMap.toList $ table

--------------------------------------------------------------------------------
-- Indexing

{- |
The options for `indexer`.
-}
data IndexerOptions = IndexerOptions

{- |
The default options for `indexer`.
-}
defaultIndexerOptions :: IndexerOptions
defaultIndexerOptions = IndexerOptions

instance Default IndexerOptions where
  def :: IndexerOptions
  def = defaultIndexerOptions

{- |
Index data from a GHC event stream.
-}
indexer ::
  (Key k) =>
  (e -> Maybe (k, v)) ->
  IndexerOptions ->
  Table k v ->
  M.ProcessT IO e Void
indexer extractKV IndexerOptions table =
  M.mapping extractKV
    ~> M.asParts
    ~> M.repeatedly (M.await >>= liftIO . uncurry (insert table))
