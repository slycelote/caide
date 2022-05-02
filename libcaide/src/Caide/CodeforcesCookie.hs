{-# LANGUAGE OverloadedStrings #-}
module Caide.CodeforcesCookie(
      getCookie
) where

import Control.Monad.Extended (forM, orThrow, throwError, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

import qualified Crypto.Cipher.AES as Crypto
import qualified Crypto.Cipher.Types as Crypto
import Crypto.Error (CryptoFailable(CryptoPassed, CryptoFailed))

import qualified Data.ByteString.Lazy.Search as BSSearch


hexEncode :: BS.ByteString -> BS8.ByteString
hexEncode = BS8.concatMap (\byte -> byte & printf "%02x" & BS8.pack)

hexDecode :: LBS8.ByteString -> Either Text BS.ByteString
hexDecode t | LBS8.length t `mod` 2 /= 0  = throwError "hex-encoded string must have even length"
hexDecode t = [0, 2 .. LBS8.length t - 1] & mapM parse <&> BS.concat
  where
    parse :: Int64 -> Either Text BS.ByteString
    parse i = do
        a <- parseChar $ LBS8.index t i
        b <- parseChar $ LBS8.index t (i + 1)
        return $ BS.singleton $ fromIntegral (a * 16 + b)

    parseChar :: Char -> Either Text Int
    parseChar c
        | c >= '0' && c <= '9' = pure $ fromEnum c - fromEnum '0'
        | c >= 'a' && c <= 'f' = pure $ fromEnum c - fromEnum 'a' + 10
        | c >= 'A' && c <= 'F' = pure $ fromEnum c - fromEnum 'A' + 10
        | otherwise = throwError $ T.singleton c <> " is not a valid hex character"


-- | Calculate a cookie required for Codeforces requests.
-- Input is Codeforces page that may contain instructions for calculating the cookie.
-- Output is either the cookie or an error message.
getCookie :: LBS8.ByteString -> Either Text BS8.ByteString
getCookie html = do
    params <- forM "abc" $ \c -> do
        let needle = c : "=toNumbers(\""
            (_, suffix) = BSSearch.breakAfter (BS8.pack needle) html
        when (LBS8.null suffix) $ throwError ("HTML doesn't contain " <> T.pack needle)
        closingQuoteIndex <- LBS8.elemIndex '"' suffix `orThrow`
            ("Couldn't find closing quote for " <> T.pack needle)
        hexDecode $ LBS8.take closingQuoteIndex suffix

    -- echo -n input | xxd -r -ps | openssl aes-128-cbc -d -K key -iv iv -nopad | xxd -ps
    let [key, ivBS, input] = params

    iv <- Crypto.makeIV ivBS `orThrow` "Couldn't create IV"
    cipher <- case Crypto.cipherInit key :: CryptoFailable Crypto.AES128 of
        CryptoPassed c -> pure c
        CryptoFailed e -> throwError $ "Initializing AES128 failed: " <> T.pack (show e)

    let plainText = Crypto.cbcDecrypt cipher iv input
    return $ "RCPC=" <> hexEncode plainText

