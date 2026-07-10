module IpeDB.Database (
  LSMTree,
) where

import Data.Machine (ProcessT, SourceT)
import Data.Vector (Vector)
import Data.Void (Void)
import IpeDB.Database.Class (Database (..))
import IpeDB.Database.LSMTree qualified as LSMTree

data LSMTree

instance Database LSMTree where
  type Key LSMTree = LSMTree.Key
  type Value LSMTree = LSMTree.Value

  type SessionOptions LSMTree = LSMTree.SessionOptions
  type Session LSMTree = LSMTree.Session

  withNewSession :: SessionOptions LSMTree -> (Session LSMTree -> IO r) -> IO r
  withNewSession = LSMTree.withNewSession

  type TableOptions LSMTree = LSMTree.TableOptions
  type Table LSMTree k v = LSMTree.Table k v

  withNewTable :: Session LSMTree -> TableOptions LSMTree -> (Table LSMTree k v -> IO r) -> IO r
  withNewTable = LSMTree.withNewTable

  inserts :: (Key LSMTree k, Value LSMTree v) => Table LSMTree k v -> Vector (k, v) -> IO ()
  inserts = LSMTree.inserts

  lookups :: (Key LSMTree k, Value LSMTree v) => Table LSMTree k v -> Vector k -> IO (Vector (Maybe v))
  lookups = LSMTree.lookups

  insert :: (Key LSMTree k, Value LSMTree v) => Table LSMTree k v -> k -> v -> IO ()
  insert = LSMTree.insert

  lookup :: (Key LSMTree k, Value LSMTree v) => Table LSMTree k v -> k -> IO (Maybe v)
  lookup = LSMTree.lookup

  type IteratorOptions LSMTree = LSMTree.IteratorOptions

  withIterator :: (Key LSMTree k, Value LSMTree v) => IteratorOptions LSMTree -> Table LSMTree k v -> (SourceT IO (k, v) -> IO a) -> IO a
  withIterator = LSMTree.withIterator

  type IndexerOptions LSMTree = LSMTree.IndexerOptions

  indexer :: (Key LSMTree k, Value LSMTree v) => (e -> Maybe (k, v)) -> IndexerOptions LSMTree -> Table LSMTree k v -> ProcessT IO e Void
  indexer = LSMTree.indexer
