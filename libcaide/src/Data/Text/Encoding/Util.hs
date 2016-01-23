{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Encoding.Util(
      describeUnicodeException
    , tryDecodeUtf8
    , safeDecodeUtf8
    , universalNewlineConversionOnInput
    , universalNewlineConversionOnOutput
) where

import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Encoding.Error (UnicodeException(..))
import qualified Data.Text as T

import System.IO (nativeNewline, Newline(CRLF))

describeUnicodeException :: UnicodeException -> T.Text
describeUnicodeException = T.pack . show

tryDecodeUtf8 :: ByteString -> Either T.Text T.Text
tryDecodeUtf8 = either (Left . describeUnicodeException) Right . decodeUtf8'

-- | If the ByteString is not a valid UTF8, its Show instance is used
safeDecodeUtf8 :: ByteString -> T.Text
safeDecodeUtf8 s = case tryDecodeUtf8 s of
    Left _ -> T.pack $ show s
    Right t -> t


nativeCallsForConversion :: Bool
nativeCallsForConversion = nativeNewline == CRLF

strLF :: T.Text
strLF = "\n"

strCRLF :: T.Text
strCRLF = "\r\n"

universalNewlineConversionOnInput :: T.Text -> T.Text
universalNewlineConversionOnInput =
  if nativeCallsForConversion
  then T.replace strCRLF strLF
  else id

universalNewlineConversionOnOutput :: T.Text -> T.Text
universalNewlineConversionOnOutput =
  if nativeCallsForConversion
  then T.replace strLF strCRLF
  else id

