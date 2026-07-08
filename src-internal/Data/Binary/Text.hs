module Data.Binary.Text (
  getStringUTF8LEB128,
  putStringUTF8LEB128,
  getTextUTF8LEB128,
  putTextUTF8LEB128,
  getByteStringLEB128,
  putByteStringLEB128,
) where

import Codec.LEB128.Generic (decodeLEB128, encodeLEB128)
import Data.Binary.Get (Get, getByteString, getWord8)
import Data.Binary.Put (Put, putByteString, putWord8)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

{- |
Decode `String` from a UTF-8 `ByteString` using a LEB128-encoded length.
-}
getStringUTF8LEB128 :: Get String
getStringUTF8LEB128 = T.unpack <$> getTextUTF8LEB128

{- |
Encode `String` into a UTF-8 `ByteString` using a LEB128-encoded length.
-}
putStringUTF8LEB128 :: String -> Put
putStringUTF8LEB128 = putTextUTF8LEB128 . T.pack

{- |
Decode `Text` from a UTF-8 `ByteString` using a LEB128-encoded length.
-}
getTextUTF8LEB128 :: Get Text
getTextUTF8LEB128 = TE.decodeUtf8 <$> getByteStringLEB128

{- |
Encode `Text` into a UTF-8 `ByteString` using a LEB128-encoded length.
-}
putTextUTF8LEB128 :: Text -> Put
putTextUTF8LEB128 = putByteStringLEB128 . TE.encodeUtf8

{- |
Encode a `ByteString` using a LEB128-encoded length.
-}
getByteStringLEB128 :: Get ByteString
getByteStringLEB128 = do
  n <- decodeLEB128 getWord8
  getByteString n

{- |
Decode a `ByteString` using a LEB128-encoded length.
-}
putByteStringLEB128 :: ByteString -> Put
putByteStringLEB128 bs = do
  encodeLEB128 putWord8 (BS.length bs)
  putByteString bs
