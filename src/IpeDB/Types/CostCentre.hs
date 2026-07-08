{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : IpeDb.Types.CostCentre
Description : Representation for GHC cost centres.
Stability   : experimental
Portability : portable
-}
module IpeDB.Types.CostCentre (
  CostCentreId (..),
  CostCentre (..),
  toCostCentre,
  defaultCostCentreLSMTreeTableSpec,
  defaultCostCentreIndexerOptions,
) where

import Control.Exception (assert)
import Data.Binary (Binary (..), Get, Put)
import Data.Binary.Text (getTextUTF8LEB128, putTextUTF8LEB128)
import Data.Coerce (coerce)
import Data.Default (Default (..))
import Data.Either (fromRight, isRight)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32)
import Database.LSMTree qualified as LSMT
import Foreign (toBool)
import Foreign.C.Types (CBool (..))
import GHC.RTS.Events (Event)
import GHC.RTS.Events qualified as E
import IpeDB.Database qualified as DB
import IpeDB.Types.SrcLoc (SrcLoc (..), parseSrcLoc)

{- |
The type of cost-centre IDs.
-}
newtype CostCentreId = CostCentreId
  { value :: Word32
  }
  deriving newtype (Show, Eq, Ord, Hashable)

{- |
The type of a cost-centre entry, as produced by the `GHC.RTS.Events.HeapProfCostCentre` event.
-}
data CostCentre = CostCentre
  { ccLabel :: !Text
  , ccModule :: !Text
  , ccSrcLoc :: !SrcLoc
  , ccIsCAF :: !Bool
  }
  deriving stock (Show, Eq)

{- |
Extract a `CostCentre` from a @ghc-events@ `Event`.
-}
toCostCentre :: Event -> Maybe (CostCentreId, CostCentre)
toCostCentre ev
  -- If the event is an `E.InfoTableProv` event, process it, and set @started@...
  | E.HeapProfCostCentre{..} <- ev.evSpec = do
      let !ccId = CostCentreId heapProfCostCentreId
      let !ccSrcLocOrError
            | heapProfSrcLoc `elem` ["<built-in>", "<entire-module>"] = Right UnhelpfulSrcLoc
            | otherwise = parseSrcLoc . T.unpack $ heapProfSrcLoc
      assert (isRight ccSrcLocOrError) $ do
        let !cc =
              CostCentre
                { ccLabel = heapProfLabel
                , ccModule = heapProfModule
                , ccSrcLoc = fromRight UnhelpfulSrcLoc ccSrcLocOrError
                , ccIsCAF = toBool (coerce @E.HeapProfFlags @CBool heapProfFlags)
                }
        Just (ccId, cc)
  | otherwise = Nothing

{- |
Default `DB.TableSpec` for CostCentre tables.
-}
defaultCostCentreLSMTreeTableSpec :: DB.TableSpec CostCentreId CostCentre
defaultCostCentreLSMTreeTableSpec =
  DB.LSMTreeTableSpec
    { name = "cost-centre-table"
    , label = "CostCentreId->CostCentre"
    }

instance Default (DB.TableSpec CostCentreId CostCentre) where
  def :: DB.TableSpec CostCentreId CostCentre
  def = defaultCostCentreLSMTreeTableSpec

{- |
Default `DB.IndexerOptions` for CostCentre tables.
-}
defaultCostCentreIndexerOptions :: DB.IndexerOptions Event CostCentreId CostCentre
defaultCostCentreIndexerOptions = DB.defaultIndexerOptions toCostCentre

instance Default (DB.IndexerOptions Event CostCentreId CostCentre) where
  def :: DB.IndexerOptions Event CostCentreId CostCentre
  def = defaultCostCentreIndexerOptions

--------------------------------------------------------------------------------
-- Instances for binary serialisation of CostCentre
--------------------------------------------------------------------------------

deriving newtype instance Binary CostCentreId

instance Binary CostCentre where
  get :: Get CostCentre
  get = do
    ccLabel <- getTextUTF8LEB128
    ccModule <- getTextUTF8LEB128
    ccSrcLoc <- get
    ccIsCAF <- get
    pure CostCentre{..}

  put :: CostCentre -> Put
  put CostCentre{..} = do
    putTextUTF8LEB128 ccLabel
    putTextUTF8LEB128 ccModule
    put ccSrcLoc
    put ccIsCAF

--------------------------------------------------------------------------------
-- Instances for lsm-tree serialisation of CostCentre
--------------------------------------------------------------------------------

deriving newtype instance LSMT.SerialiseKey CostCentreId

deriving via DB.SerialiseViaBinary CostCentre instance LSMT.SerialiseValue CostCentre

deriving via LSMT.ResolveAsFirst CostCentre instance LSMT.ResolveValue CostCentre
