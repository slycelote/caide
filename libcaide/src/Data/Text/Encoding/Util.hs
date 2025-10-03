{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Encoding.Util(
      tryDecodeUtf8
    , universalNewlineConversionOnInput
    , universalNewlineConversionOnOutput
    , module Data.Text.Encoding
) where

import Control.Exception (displayException)
import Data.ByteString (ByteString)
import Data.Text.Encoding
import qualified Data.Text as T

import System.IO (nativeNewline, Newline(CRLF))


tryDecodeUtf8 :: ByteString -> Either T.Text T.Text
tryDecodeUtf8 = either (Left . T.pack . displayException) Right . decodeUtf8'

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

