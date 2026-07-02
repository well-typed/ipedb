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
  defaultInfoProvLSMTreeTableSpec,
  defaultInfoProvIndexerOptions,
) where

import Control.Exception (assert)
import Data.Binary (Binary (..), Get, Put)
import Data.Binary.Text (getTextUtf8, putTextUtf8)
import Data.Default (Default (..))
import Data.Either (fromRight, isRight)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Database.LSMTree qualified as LSMT
import GHC.RTS.Events (Event)
import GHC.RTS.Events qualified as E
import IpeDB.Database qualified as DB
import IpeDB.Types.SrcLoc (SrcLoc (..), parseSrcLoc)
import Numeric (showHex)
import Text.ParserCombinators.ReadP qualified as P
import Text.Read.Lex (readHexP)

{- |
The type of info table provenance IDs.
-}
newtype InfoProvId = InfoProvId Word64
  deriving newtype (Eq, Hashable, Ord)

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
  deriving (Show, Eq)

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

{- |
Default `DB.TableSpec` for CostCentre tables.
-}
defaultInfoProvLSMTreeTableSpec :: DB.TableSpec InfoProvId InfoProv
defaultInfoProvLSMTreeTableSpec =
  DB.LSMTreeTableSpec
    { name = "info-prov-table"
    , label = "InfoProvId->InfoProv"
    }

instance Default (DB.TableSpec InfoProvId InfoProv) where
  def :: DB.TableSpec InfoProvId InfoProv
  def = defaultInfoProvLSMTreeTableSpec

{- |
Default `DB.IndexerOptions` for InfoProv tables.
-}
defaultInfoProvIndexerOptions :: DB.IndexerOptions Event InfoProvId InfoProv
defaultInfoProvIndexerOptions = DB.defaultIndexerOptions toInfoProv

instance Default (DB.IndexerOptions Event InfoProvId InfoProv) where
  def :: DB.IndexerOptions Event InfoProvId InfoProv
  def = defaultInfoProvIndexerOptions

--------------------------------------------------------------------------------
-- Instances for binary serialisation of InfoProv
--------------------------------------------------------------------------------

deriving newtype instance Binary InfoProvId

instance Binary InfoProv where
  get :: Get InfoProv
  get = do
    ipName <- getTextUtf8
    ipClosureDesc <- get
    ipTyDesc <- getTextUtf8
    ipLabel <- getTextUtf8
    ipModule <- getTextUtf8
    ipSrcLoc <- get
    pure InfoProv{..}

  put :: InfoProv -> Put
  put InfoProv{..} = do
    putTextUtf8 ipName
    put ipClosureDesc
    putTextUtf8 ipTyDesc
    putTextUtf8 ipLabel
    putTextUtf8 ipModule
    put ipSrcLoc

--------------------------------------------------------------------------------
-- Instances for lsm-tree serialisation of InfoProv
--------------------------------------------------------------------------------

deriving newtype instance LSMT.SerialiseKey InfoProvId

deriving via DB.SerialiseViaBinary InfoProv instance LSMT.SerialiseValue InfoProv

deriving via LSMT.ResolveAsFirst InfoProv instance LSMT.ResolveValue InfoProv
