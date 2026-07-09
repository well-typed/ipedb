{- |
Module      : IpeDB.Types.InfoProv
Description : Representation for GHC info table provenance entries.
Stability   : experimental
Portability : portable
-}
module IpeDB.Types.InfoProv (
  InfoProvId (..),
  InfoProv (..),
  toInfoProv,
) where

import Control.Exception (assert)
import Data.Binary (Binary (..), Get, Put)
import Data.Binary.Text (getTextUTF8LEB128, putTextUTF8LEB128)
import Data.Either (fromRight, isRight)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Database.LSMTree qualified as LSMT
import GHC.Generics (Generic)
import GHC.RTS.Events (Event)
import GHC.RTS.Events qualified as E
import IpeDB.Database.LSMTree qualified as DB (SerialiseViaBinary (..), Value)
import IpeDB.Types.SrcLoc (SrcLoc (..), parseSrcLoc)
import Numeric (showHex)
import Text.ParserCombinators.ReadP qualified as P
import Text.Read.Lex (readHexP)

{- |
The type of info table provenance IDs.
-}
newtype InfoProvId = InfoProvId Word64
  deriving newtype (Hashable, Num)
  deriving stock (Generic, Eq, Ord)

instance Show InfoProvId where
  showsPrec :: Int -> InfoProvId -> ShowS
  showsPrec _ (InfoProvId ipId) = showString "0x" . showHex ipId

instance Read InfoProvId where
  readsPrec :: Int -> ReadS InfoProvId
  readsPrec _ = P.readP_to_S (InfoProvId <$> (P.string "0x" *> readHexP))

{- |
The type of an info table provenance entry, as produced by the `GHC.RTS.Events.InfoTableProv` event.
-}
data InfoProv = InfoProv
  { ipName :: !Text
  , ipClosureDesc :: !Int
  , ipTyDesc :: !Text
  , ipLabel :: !Text
  , ipModule :: !Text
  , ipSrcLoc :: !SrcLoc
  }
  deriving stock (Generic, Eq, Ord, Show, Read)

{- |
Extract an `InfoProv` from a @ghc-events@ `Event`.
-}
toInfoProv :: Event -> Maybe (InfoProvId, InfoProv)
toInfoProv ev
  -- If the event is an `E.InfoTableProv` event, process it, and set @started@...
  | E.InfoTableProv{..} <- ev.evSpec = do
      let !ipId = InfoProvId itInfo
      let !ipSrcLocOrError = parseSrcLoc . T.unpack $ itSrcLoc
      assert (isRight ipSrcLocOrError) $ do
        let !ip =
              InfoProv
                { ipName = itTableName
                , ipClosureDesc = itClosureDesc
                , ipTyDesc = itTyDesc
                , ipLabel = itLabel
                , ipModule = itModule
                , ipSrcLoc = fromRight UnhelpfulSrcLoc ipSrcLocOrError
                }
        Just (ipId, ip)
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Instances for binary serialisation of InfoProv
--------------------------------------------------------------------------------

deriving newtype instance Binary InfoProvId

instance Binary InfoProv where
  get :: Get InfoProv
  get = do
    ipName <- getTextUTF8LEB128
    ipClosureDesc <- get
    ipTyDesc <- getTextUTF8LEB128
    ipLabel <- getTextUTF8LEB128
    ipModule <- getTextUTF8LEB128
    ipSrcLoc <- get
    pure InfoProv{..}

  put :: InfoProv -> Put
  put InfoProv{..} = do
    putTextUTF8LEB128 ipName
    put ipClosureDesc
    putTextUTF8LEB128 ipTyDesc
    putTextUTF8LEB128 ipLabel
    putTextUTF8LEB128 ipModule
    put ipSrcLoc

--------------------------------------------------------------------------------
-- Instances for lsm-tree serialisation of InfoProv
--------------------------------------------------------------------------------

deriving newtype instance LSMT.SerialiseKey InfoProvId

deriving via DB.SerialiseViaBinary InfoProv instance DB.Value InfoProv

deriving via DB.SerialiseViaBinary InfoProv instance LSMT.SerialiseValue InfoProv

deriving via LSMT.ResolveAsFirst InfoProv instance LSMT.ResolveValue InfoProv
