{-# LANGUAGE CPP, OverloadedStrings #-}
module Network.HTTP.Util(
      downloadDocument
    , httpPost
) where

import Control.Exception (catch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Text (Text)
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..), Request,
    defaultRequest,
    httpLbs, newManager, parseUrlThrow,
    host, method, path, port, queryString, requestHeaders, requestBody, secure,
    RequestBody(RequestBodyLBS),
    Response, responseStatus, responseBody, responseTimeout, responseTimeoutMicro, )

#ifdef VERSION_http_client_openssl
import Network.HTTP.Client.OpenSSL (newOpenSSLManager, withOpenSSL)
#else
import Network.Connection (TLSSettings(TLSSettingsSimple))
import Network.HTTP.Client.TLS (mkManagerSettings)
#endif
import Network.HTTP.Types.Header (Header, RequestHeaders, hAccept, hAcceptEncoding, hCookie, hOrigin, hUserAgent)
import Network.HTTP.Types.Status (ok200, statusCode, statusMessage)
import Network.URI(URI(uriPath, uriQuery, uriFragment))
import System.IO.Error (catchIOError, ioeGetErrorString)

import Data.Text.Encoding.Util (safeDecodeUtf8, tryDecodeUtf8)

seconds :: Int
seconds = 1000 * 1000

{- | Download an HTML document. Return (Left errorMessage) in case of an error,
     (Right doc) in case of success.
-}
downloadDocument :: Text -> IO (Either Text Text)
downloadDocument url
    | isNothing mbRequest       = return . Left $ "URL not supported"
    | otherwise                 = do
          mbResponseBody <- getResponseBody request'
          return $ case mbResponseBody of
              Left err   -> Left err
              Right body -> tryDecodeUtf8 . LBS.toStrict $ body
  where
    mbRequest = parseUrlThrow $ T.unpack url
    Just request = mbRequest
    request' = request {
        responseTimeout = responseTimeoutMicro $ 10*seconds,
        requestHeaders  =
            [ (hAcceptEncoding, "") -- omit this header
            , (hUserAgent, "Mozilla/5.0 (Windows NT 10.0; rv:78.0) Gecko/20100101 Firefox/78.0")
            , (hAccept, "*/*")
            ] ++ getAdditionalHeaders (host request)
    }

getAdditionalHeaders :: BS.ByteString -> RequestHeaders
getAdditionalHeaders requestHost =
    [(hCookie, "RCPC=6b9ade1a791972f03788f4fe51b5b8e8") |
        requestHost `elem` ["codeforces.com", "www.codeforces.com"]]

httpPost :: URI -> LBS.ByteString -> [Header] -> IO (Either Text LBS.ByteString)
httpPost uri body headers
    | isNothing mbRequest       = return . Left $ "URL not supported"
    | otherwise                 = getResponseBody request'
  where
    mbRequest = parseUrlThrow $ show uri
    Just request = mbRequest
    request' = request
            { requestBody = RequestBodyLBS body
            , method = "POST"
            , responseTimeout = responseTimeoutMicro $ 5*seconds
            , requestHeaders = headers <>
                [ (hUserAgent, "Mozilla/5.0 (Windows NT 10.0; x86_64; rv:91.0) Gecko/20100101 Firefox/91.0")
                , (hAccept, "*/*")
                , (hOrigin, BS8.pack . show $ uri{uriPath="", uriQuery="", uriFragment=""})
                ]
            }

describeHttpException :: HttpException -> Text
describeHttpException (InvalidUrlException url reason) = "URL '" <> T.pack url <> "' is invalid: " <> T.pack reason
describeHttpException (HttpExceptionRequest _ content) = describeExceptionContent content

describeExceptionContent :: HttpExceptionContent -> Text
describeExceptionContent (StatusCodeException response _) =
    "HTTP status " <> tshow (statusCode status) <> ": " <> tshow (statusMessage status)
  where
    status = responseStatus response

describeExceptionContent (TooManyRedirects _) = "Too many redirects"
describeExceptionContent e = tshow e

tshow :: Show a => a -> Text
tshow = T.pack . show

statusExceptionHandler :: Request -> HttpException -> IO (Either Text a)
statusExceptionHandler request e = return . Left $ "HTTP error for URL '" <> url <> "'. " <> describeHttpException e
  where
    url = T.pack . BS8.unpack $
        "http" <> (if secure request then "s" else "") <> "://" <> host request <> ":" <> BS8.pack (show $ port request) <> path request <> queryString request

errorHandler :: IOError -> IO (Either Text a)
errorHandler = return . Left . T.pack . ioeGetErrorString

runRequest :: Request -> IO (Either Text (Response LBS.ByteString))
runRequest request = (do
#ifdef VERSION_http_client_openssl
    manager <- withOpenSSL newOpenSSLManager
#else
    let tlsManagerSettings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- newManager tlsManagerSettings
#endif
    response <- httpLbs request manager
    return $ Right response
    ) `catchIOError` errorHandler `catch` statusExceptionHandler request

getResponseBody :: Request -> IO (Either Text LBS.ByteString)
getResponseBody request = do
    mbResponse <- runRequest request
    return $ case mbResponse of
        Left err -> Left err
        Right response -> let status = responseStatus response in
            if status == ok200
            then Right $ responseBody response
            else Left . safeDecodeUtf8 . statusMessage $ status

