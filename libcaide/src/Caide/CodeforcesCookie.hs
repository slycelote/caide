{-# LANGUAGE OverloadedStrings #-}
module Caide.CodeforcesCookie(
      getCookie
    , newHttpMiddleware
) where

import Control.Monad.Extended (forM, orThrow, throwError, when)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, swapMVar)
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

import Network.HTTP.Types.Header (hCookie)
import Network.HTTP.Types.Status (statusIsSuccessful)
import qualified Caide.HttpClient as Http


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


data CookieStatus = Unknown
                  | NotRequired
                  | Required !BS8.ByteString

newHttpMiddleware :: IO Http.Middleware
newHttpMiddleware = do
    -- TODO: Cache the cookie in settings and get rid of Unknown status.
    status <- newMVar Unknown
    return $ Http.middleware $ \send request ->
        if Http.host request `elem` ["codeforces.com", "www.codeforces.com",
            "codeforces.ru", "www.codeforces.ru", "codeforces.ml", "www.codeforces.ml"]
        then cfsend send status request
        else send request

cfsend :: (Http.Request -> IO Http.ResponseLBS) -> MVar CookieStatus -> Http.Request -> IO Http.ResponseLBS
cfsend send statusMVar request = do
    let sendWith :: Maybe BS8.ByteString -> IO Http.ResponseLBS
        sendWith Nothing = send request
        sendWith (Just cookie) = send $
            request{Http.requestHeaders = (hCookie, cookie) : Http.requestHeaders request}

    (cookieOrResponse, ranFirstRequest) <- modifyMVar statusMVar $ \s -> case s of
        NotRequired     -> pure $ (s, (Left Nothing,       False))
        Required cookie -> pure $ (s, (Left (Just cookie), False))
        Unknown -> do
            -- This is the first request to Codeforces. Block all other requests
            -- while we're executing.
            response <- sendWith Nothing
            let failed = not $ statusIsSuccessful $ Http.responseStatus response
            case getCookie (Http.responseBody response) of
                Right cookie       -> pure (Required cookie, (Left (Just cookie), True))
                Left _err | failed -> pure (Unknown,         (Right response,     True))
                Left _err          -> pure (NotRequired,     (Right response,     True))

    case cookieOrResponse of
        Right response -> pure response -- Our first request succeeded.
        Left cookie -> do
            -- Either we or another thread found the cookie.
            response <- sendWith cookie
            case getCookie (Http.responseBody response) of
                Left _err ->
                    -- Succeeded.
                    pure response

                Right newCookie | ranFirstRequest || Just newCookie == cookie ->
                    -- Failed despite setting the correct cookie.
                    pure response

                Right newCookie -> do
                    -- New cookie is required.
                    _ <- swapMVar statusMVar $ Required newCookie
                    sendWith (Just newCookie)

