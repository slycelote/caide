{-# LANGUAGE CPP, OverloadedStrings #-}
module Network.HTTP.Util(
      downloadDocument
) where

import Control.Exception (catch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..), Request,
    httpLbs, newManager, parseUrlThrow,
    host, requestHeaders,
    responseStatus, responseBody, responseTimeout, responseTimeoutMicro, )

#ifdef VERSION_http_client_openssl
import Network.HTTP.Client.OpenSSL (newOpenSSLManager, withOpenSSL)
#else
import Network.Connection (TLSSettings(TLSSettingsSimple))
import Network.HTTP.Client.TLS (mkManagerSettings)
#endif
import Network.HTTP.Types.Header (RequestHeaders, hAccept, hAcceptEncoding, hCookie, hUserAgent)
import Network.HTTP.Types.Status (ok200, statusCode, statusMessage)
import System.IO.Error (catchIOError, ioeGetErrorString)

import Data.Text.Encoding.Util (safeDecodeUtf8, tryDecodeUtf8)

{- | Download an HTML document. Return (Left errorMessage) in case of an error,
     (Right doc) in case of success.
-}
downloadDocument :: T.Text -> IO (Either T.Text T.Text)
downloadDocument url
    | isNothing mbRequest       = mkLiftedError "URL not supported"
    | otherwise                 = result
  where
    mbRequest = parseUrlThrow $ T.unpack url
    Just request = mbRequest
    seconds = 1000 * 1000
    request' = request {
        responseTimeout = responseTimeoutMicro $ 10*seconds,
        requestHeaders  =
            [ (hAcceptEncoding, "") -- omit this header
            , (hUserAgent, "Mozilla/5.0 (Windows NT 10.0; rv:78.0) Gecko/20100101 Firefox/78.0")
            , (hAccept, "*/*")
            ] ++ getAdditionalHeaders (host request)
    }
    mkLiftedError = return . Left
    errorHandler = mkLiftedError . T.pack . ioeGetErrorString
    result = httpDownloader request' `catchIOError` errorHandler `catch` statusExceptionHandler url

getAdditionalHeaders :: BS.ByteString -> RequestHeaders
getAdditionalHeaders requestHost =
    [(hCookie, "RCPC=4f698e716ffeabe9943d7f1e60e50a0b") |
        requestHost `elem` ["codeforces.com", "www.codeforces.com"]]

describeHttpException :: HttpException -> T.Text
describeHttpException (InvalidUrlException url reason) = T.concat ["URL '", T.pack url, "' is invalid: ", T.pack reason]
describeHttpException (HttpExceptionRequest _ content) = describeExceptionContent content

describeExceptionContent :: HttpExceptionContent -> T.Text
describeExceptionContent (StatusCodeException response _) =
    T.concat ["HTTP status ", tshow (statusCode status), ": ", tshow (statusMessage status)]
  where
    status = responseStatus response

describeExceptionContent (TooManyRedirects _) = "Too many redirects"
describeExceptionContent e = tshow e

tshow :: Show a => a -> T.Text
tshow = T.pack . show

statusExceptionHandler :: T.Text -> HttpException -> IO (Either T.Text T.Text)
statusExceptionHandler url e = return . Left $ T.concat ["Error fetching URL '", url, "'. ", describeHttpException e]

httpDownloader :: Request -> IO (Either T.Text T.Text)
httpDownloader request = do
#ifdef VERSION_http_client_openssl
    manager <- withOpenSSL newOpenSSLManager
#else
    let tlsManagerSettings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- newManager tlsManagerSettings
#endif
    response <- httpLbs request manager
    let status = responseStatus response
    return $ if status == ok200
       then tryDecodeUtf8 . LBS.toStrict . responseBody $ response
       else Left . safeDecodeUtf8 . statusMessage $ status

