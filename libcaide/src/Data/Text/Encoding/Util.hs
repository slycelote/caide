module Data.Text.Encoding.Util(
      describeUnicodeException
    , tryDecodeUtf8
) where

import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Encoding.Error (UnicodeException(..))
import qualified Data.Text as T

describeUnicodeException :: UnicodeException -> T.Text
describeUnicodeException (DecodeError s _) = T.pack s
describeUnicodeException (EncodeError s _) = T.pack s

tryDecodeUtf8 :: ByteString -> Either T.Text T.Text
tryDecodeUtf8 = either (Left . describeUnicodeException) Right . decodeUtf8'

