{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
Module      : IpeDB.Database.Class
Description : This modules provides a generic abstraction over databases.
Stability   : experimental
Portability : portable
-}
module IpeDB.Database.Class (
  -- * Class
  Database (..),

  -- * Serialisation
  SerialiseVia (..),

  -- * Errors
  TargetExistsError (..),
) where

import Control.Exception (Exception (..))
import Control.Monad ((<=<))
import Data.Default (Default)
import Data.Foldable (traverse_)
import Data.Kind (Constraint, Type)
import Data.Machine (SourceT)
import Data.Machine.Process (ProcessT)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Void (Void)
import Prelude hiding (lookup)

class
  ( Default (SessionOptions b)
  , Default (TableOptions b)
  , Default (IteratorOptions b)
  , Default (IndexerOptions b)
  ) =>
  Database b
  where
  {-# MINIMAL
    withNewSession
    , withNewTable
    , (inserts | insert)
    , (lookups | lookup)
    , withIterator
    , indexer
    #-}

  type Key b k :: Constraint
  type Value b v :: Constraint

  type SessionOptions b :: Type
  type Session b :: Type

  withNewSession ::
    SessionOptions b ->
    (Session b -> IO r) ->
    IO r

  type TableOptions b :: Type
  type Table b k v :: Type

  withNewTable ::
    Session b ->
    TableOptions b ->
    (Table b k v -> IO r) ->
    IO r

  inserts ::
    (Key b k, Value b v) =>
    Table b k v ->
    Vector (k, v) ->
    IO ()
  inserts t = traverse_ (uncurry $ insert @b t)

  lookups ::
    (Key b k, Value b v) =>
    Table b k v ->
    Vector k ->
    IO (Vector (Maybe v))
  lookups t = traverse (lookup @b t)

  insert ::
    (Key b k, Value b v) =>
    Table b k v ->
    k ->
    v ->
    IO ()
  insert t k v = inserts @b t (V.singleton (k, v))

  lookup ::
    forall k v.
    (Key b k, Value b v) =>
    Table b k v ->
    k ->
    IO (Maybe v)
  lookup t = fmap (fst <=< V.uncons) . lookups @b t . V.singleton

  type IteratorOptions b :: Type

  withIterator ::
    (Key b k, Value b v) =>
    IteratorOptions b ->
    Table b k v ->
    (SourceT IO (k, v) -> IO a) ->
    IO a

  type IndexerOptions b :: Type

  indexer ::
    (Key b k, Value b v) =>
    (e -> Maybe (k, v)) ->
    IndexerOptions b ->
    Table b k v ->
    ProcessT IO e Void

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

{- |
Wrapper that derives the constraints required by the database backend via `Coercible`.
-}
newtype SerialiseVia v u = SerialiseVia {value :: v}

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

{- |
This error is raised if the target path already exists.
-}
newtype TargetExistsError = TargetExistsError FilePath
  deriving (Show)

instance Exception TargetExistsError
