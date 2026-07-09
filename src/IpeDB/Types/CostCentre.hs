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
) where

import Control.Exception (assert)
import Data.Binary (Binary (..), Get, Put)
import Data.Binary.Text (getTextUTF8LEB128, putTextUTF8LEB128)
import Data.Coerce (coerce)
import Data.Either (fromRight, isRight)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32)
import Database.LSMTree qualified as LSMT
import Foreign (toBool)
import Foreign.C.Types (CBool (..))
import GHC.Generics (Generic)
import GHC.RTS.Events (Event)
import GHC.RTS.Events qualified as E
import IpeDB.Database.Class qualified as DB
import IpeDB.Database.LSMTree ()
import IpeDB.Types.SrcLoc (SrcLoc (..), parseSrcLoc)
import Numeric (showHex)
import Text.ParserCombinators.ReadP qualified as P
import Text.Read.Lex (readHexP)

{- |
The type of cost-centre IDs.
-}
newtype CostCentreId = CostCentreId
  { value :: Word32
  }
  deriving newtype (Hashable, Num)
  deriving stock (Generic, Eq, Ord)

instance Show CostCentreId where
  showsPrec :: Int -> CostCentreId -> ShowS
  showsPrec _ (CostCentreId ipId) = showString "0x" . showHex ipId

instance Read CostCentreId where
  readsPrec :: Int -> ReadS CostCentreId
  readsPrec _ = P.readP_to_S (CostCentreId <$> (P.string "0x" *> readHexP))

{- |
The type of a cost-centre entry, as produced by the `GHC.RTS.Events.HeapProfCostCentre` event.
-}
data CostCentre = CostCentre
  { ccLabel :: !Text
  , ccModule :: !Text
  , ccSrcLoc :: !SrcLoc
  , ccIsCAF :: !Bool
  }
  deriving stock (Generic, Eq, Ord, Show, Read)

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
