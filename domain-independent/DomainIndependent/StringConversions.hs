module DomainIndependent.StringConversions
  ( stringToStrictByteString,
    lazyByteStringToText,
    textToString,
    stringToText,
    stringToLazyByteString,
    strictByteStringToString,
    textToLazyByteString,
    textToStrictByteString,
    lazyByteStringToString,
    strictByteStringToText,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Conversion as BConv
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding
  ( decodeUtf8,
    encodeUtf8,
  )

stringToLazyByteString :: String -> BL.ByteString
stringToLazyByteString = BConv.toByteString

stringToStrictByteString :: String -> BS.ByteString
stringToStrictByteString = encodeUtf8 . Text.pack

lazyByteStringToText :: BL.ByteString -> Text
lazyByteStringToText = decodeUtf8 . BL.toStrict

lazyByteStringToString :: BL.ByteString -> String
lazyByteStringToString = Text.unpack . lazyByteStringToText

textToString :: Text -> String
textToString = Text.unpack

stringToText :: String -> Text
stringToText = Text.pack

textToLazyByteString :: Text -> BL.ByteString
textToLazyByteString = BL.fromStrict . encodeUtf8

textToStrictByteString :: Text -> BS.ByteString
textToStrictByteString = encodeUtf8

strictByteStringToString :: BS.ByteString -> String
strictByteStringToString = B8.unpack

strictByteStringToText :: BS.ByteString -> Text
strictByteStringToText = stringToText . strictByteStringToString
