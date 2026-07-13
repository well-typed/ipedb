{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : IpeDB.Types.SrcLoc
Description : Representation for GHC source locations.
Stability   : experimental
Portability : portable
-}
module IpeDB.Types.SrcLoc (
  SrcLoc (SrcLoc, UnhelpfulSrcLoc, srcFilePath, srcRange),
  ppSrcLoc,
  parseSrcLoc,
  Range (Range, start, end, ..),
  ppRange,
  parseRange,
  Point (..),
) where

import Codec.LEB128.Generic (decodeLEB128, encodeLEB128)
import Data.Binary (Binary (..), Get, Put, getWord8, putWord8)
import Data.Binary.Text (getStringUTF8LEB128, putStringUTF8LEB128)
import Data.Char (isDigit)
import Data.List qualified as L
import Data.Word (Word32)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as P

--------------------------------------------------------------------------------
-- Source Locations
--------------------------------------------------------------------------------

{- |
The type of source location information.
-}
data SrcLoc = SrcLoc
  { srcFilePath :: !FilePath
  , srcRange :: !(Maybe Range)
  }
  deriving stock (Generic, Eq, Ord, Show, Read)

{- |
Simple constructor for unhelpful source locations.
-}
pattern UnhelpfulSrcLoc :: SrcLoc
pattern UnhelpfulSrcLoc = SrcLoc "" Nothing

{- |
Pretty-printer for GHC source location information
-}
ppSrcLoc :: SrcLoc -> ShowS
ppSrcLoc SrcLoc{..} =
  showString srcFilePath . showString ":" . maybe mempty ppRange srcRange

{- |
Parser for GHC source location information.
-}
parseSrcLoc :: String -> Either String SrcLoc
parseSrcLoc str0 = parseSrcLocRev (reverse str0)
 where
  parseSrcLocRev :: String -> Either String SrcLoc
  parseSrcLocRev strRev0 = do
    let (maybeSrcRangeRevStr0, strRev1) = span (/= ':') strRev0
    let stripColon = \case (':' : str) -> str; str -> str
    let strRev2 = stripColon strRev1
    case maybeSrcRangeRevStr0 of
      -- If this SrcLoc uses the empty range...
      [] | ":" `L.isPrefixOf` strRev1 -> do
        let srcFilePath = reverse strRev2
        let srcRange = Nothing
        pure SrcLoc{..}
      -- If this SrcLoc uses a multiline range...
      ')' : _ -> do
        let srcFilePath = reverse strRev2
        srcRange <- Just <$> parseRange (reverse maybeSrcRangeRevStr0)
        pure SrcLoc{..}
      -- If this SrcLoc uses a oneline or point range...
      (c : _) | isDigit c -> do
        let (maybeSrcRangeRevStr1, strRev3) = span (/= ':') strRev2
        let srcFilePath = reverse (stripColon strRev3)
        srcRange <- Just <$> parseRange (reverse maybeSrcRangeRevStr1 <> ":" <> reverse maybeSrcRangeRevStr0)
        pure SrcLoc{..}
      -- Otherwise, this SrcLoc has an invalid range...
      _otherwise -> do
        Left "parseSrcLoc: no parse"

--------------------------------------------------------------------------------
-- Source Ranges
--------------------------------------------------------------------------------

{- |
A position in a source file.
-}
data Point
  = Point
  { line :: !Word32
  , column :: !Word32
  }
  deriving stock (Generic, Eq, Ord, Show, Read)

{- |
A range in a source file.
-}
data Range
  = Range'Point
      { line :: !Word32
      , column :: !Word32
      }
  | Range'OneLine
      { line :: !Word32
      , column :: !Word32
      , endColumn :: !Word32
      }
  | Range'MultiLine
      { line :: !Word32
      , column :: !Word32
      , endLine :: !Word32
      , endColumn :: !Word32
      }
  deriving stock (Generic, Eq, Ord, Show, Read)

{- |
Smart constructor for ranges.
-}
toRange :: (Point, Point) -> Range
toRange (start@Point{..}, end)
  | start == end = Range'Point{..}
  | line == end.line = Range'OneLine{endColumn = end.column, ..}
  | otherwise = Range'MultiLine{endLine = end.line, endColumn = end.column, ..}

{- |
Smart destructor for ranges.
-}
fromRange :: Range -> (Point, Point)
fromRange r = (r.start, r.end)

{- |
Smart constructor for ranges.
-}
pattern Range :: Point -> Point -> Range
pattern Range{start, end} <- (fromRange -> (start, end))
  where
    Range start end = toRange (start, end)

{-# COMPLETE Range #-}

{- |
Get the start position of a range.
-}
instance HasField "start" Range Point where
  getField :: Range -> Point
  getField = \case
    Range'Point{..} -> Point{..}
    Range'OneLine{..} -> Point{..}
    Range'MultiLine{..} -> Point{..}

{- |
Get the end position of a range.
-}
instance HasField "end" Range Point where
  getField :: Range -> Point
  getField = \case
    Range'Point{..} -> Point{..}
    Range'OneLine{..} -> Point{column = endColumn, ..}
    Range'MultiLine{..} -> Point{line = endLine, column = endColumn}

instance Semigroup Range where
  (<>) :: Range -> Range -> Range
  r1 <> r2 = Range (r1.start `min` r2.start) (r1.end `max` r2.end)

{- Note [Parsing and Pretty-Printing]
The parser and pretty-printer are based on the following pretty-printer for @RealSrcSpan@ in the GHC source:
https://github.com/ghc/ghc/blob/674858e3fb1b7fe644d7ec5d4b0c6d9f18d7bc51/compiler/GHC/Types/SrcLoc.hs#L752-L774
-}

{- |
Pretty-printer for ranges.
-}
ppRange :: Range -> ShowS
ppRange = \case
  Range'Point{..} ->
    shows line . showString ":" . shows column
  Range'OneLine{..} ->
    shows line . showString ":" . shows column . showString "-" . shows endColumn
  Range{..} ->
    ppPoint start . showString "-" . ppPoint end
 where
  ppPoint :: Point -> ShowS
  ppPoint p =
    showParen True (shows p.line . showString "," . shows p.column)

{- |
Internal helper.

Parser for ranges.
-}
pRange :: ReadP Range
pRange = pRange'MultiLine P.<++ (pRange'OneLine P.+++ pPointRange)
 where
  pRange'MultiLine :: ReadP Range
  pRange'MultiLine = Range <$> pPoint <* P.char '-' <*> pPoint

  pRange'OneLine :: ReadP Range
  pRange'OneLine = Range'OneLine <$> pIndex <* P.char ':' <*> pIndex <* P.char '-' <*> pIndex

  pPointRange :: ReadP Range
  pPointRange = Range'Point <$> pIndex <* P.char ':' <*> pIndex

  pIndex :: ReadP Word32
  pIndex = read <$> P.munch1 isDigit

  pPoint :: ReadP Point
  pPoint = Point <$ P.char '(' <*> pIndex <* P.char ',' <*> pIndex <* P.char ')'

{- |
Parse a source range.
-}
parseRange :: String -> Either String Range
parseRange s =
  case [x | (x, "") <- P.readP_to_S pRange s] of
    [x] -> Right x
    [] -> Left "parseSrcLoc: no parse"
    _ -> Left "parseSrcLoc: ambiguous parse"

--------------------------------------------------------------------------------
-- Instances for binary serialisation of SrcLoc
--------------------------------------------------------------------------------

{- |
Internal helper.

Serialise source location information.
-}
putSrcLoc :: SrcLoc -> Put
putSrcLoc SrcLoc{..} = do
  putStringUTF8LEB128 srcFilePath
  putMaybeRange srcRange

{- |
Internal helper.

Deserialise source location information.
-}
getSrcLoc :: Get SrcLoc
getSrcLoc = do
  srcFilePath <- getStringUTF8LEB128
  srcRange <- getMaybeRange
  pure SrcLoc{..}

instance Binary SrcLoc where
  put :: SrcLoc -> Put
  put = putSrcLoc
  get :: Get SrcLoc
  get = getSrcLoc

--------------------------------------------------------------------------------
-- Instances for binary serialisation of optional ranges
--------------------------------------------------------------------------------

{- |
Internal helper.

Constructor tags for binary serialisation of ranges.
-}
data MaybeRangeTag
  = TagNothing
  | TagJustRange'Point
  | TagJustRange'OneLine
  | TagJustRange'MultiLine

{- |
Internal helper.

Serialise a constructor tag for an optional range.
-}
putMaybeRangeTag :: MaybeRangeTag -> Put
putMaybeRangeTag = \case
  TagNothing -> putWord8 0x01
  TagJustRange'Point -> putWord8 0x02
  TagJustRange'OneLine -> putWord8 0x03
  TagJustRange'MultiLine -> putWord8 0x04

{- |
Internal helper.

Deserialise a constructor tag for an optional range.
-}
getMaybeRangeTag :: Get MaybeRangeTag
getMaybeRangeTag = do
  maybeRangeTag <- getWord8
  case maybeRangeTag of
    0x01 -> pure TagNothing
    0x02 -> pure TagJustRange'Point
    0x03 -> pure TagJustRange'OneLine
    0x04 -> pure TagJustRange'MultiLine
    _otherwise -> fail $ "Unexpected tag " <> show maybeRangeTag

{- |
Internal helper.

Serialise an optional range to binary.
-}
putMaybeRange :: Maybe Range -> Put
putMaybeRange = \case
  Nothing ->
    putMaybeRangeTag TagNothing
  Just Range'Point{..} -> do
    putMaybeRangeTag TagJustRange'Point
    encodeLEB128 putWord8 line
    encodeLEB128 putWord8 column
  Just Range'OneLine{..} -> do
    putMaybeRangeTag TagJustRange'OneLine
    encodeLEB128 putWord8 line
    encodeLEB128 putWord8 column
    encodeLEB128 putWord8 endColumn
  Just Range'MultiLine{..} -> do
    putMaybeRangeTag TagJustRange'MultiLine
    encodeLEB128 putWord8 line
    encodeLEB128 putWord8 column
    encodeLEB128 putWord8 endLine
    encodeLEB128 putWord8 endColumn

{- |
Internal helper.

Deserialise an optional range from binary.
-}
getMaybeRange :: Get (Maybe Range)
getMaybeRange =
  getMaybeRangeTag >>= \case
    TagNothing ->
      pure Nothing
    TagJustRange'Point -> do
      line <- decodeLEB128 getWord8
      column <- decodeLEB128 getWord8
      pure . Just $! Range'Point{..}
    TagJustRange'OneLine -> do
      line <- decodeLEB128 getWord8
      column <- decodeLEB128 getWord8
      endColumn <- decodeLEB128 getWord8
      pure . Just $! Range'OneLine{..}
    TagJustRange'MultiLine -> do
      line <- decodeLEB128 getWord8
      column <- decodeLEB128 getWord8
      endLine <- decodeLEB128 getWord8
      endColumn <- decodeLEB128 getWord8
      pure . Just $! Range'MultiLine{..}
