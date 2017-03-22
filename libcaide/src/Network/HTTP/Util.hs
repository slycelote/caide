{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Util(
      downloadDocument
) where

import Control.Exception (catch)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Network.HTTP.Client (HttpException(..), httpLbs, newManager, parseUrl,
                            responseStatus, responseBody, responseTimeout, requestHeaders, Request)
import Network.Connection (TLSSettings(TLSSettingsSimple))
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Types.Header (hAccept, hAcceptEncoding, hConnection, hUserAgent)
import Network.HTTP.Types.Status (ok200, statusMessage)
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
    mbRequest = parseUrl $ T.unpack url
    Just request = mbRequest
    request' = request {
        responseTimeout = Just (15*1000*1000), -- 15 seconds
        requestHeaders  =
            [ (hUserAgent, "wget")
            , (hAccept, "*/*")
            , (hAcceptEncoding, "")
            , (hConnection, "Keep-Alive")
            ]
    }
    mkLiftedError = return . Left
    errorHandler = mkLiftedError . T.pack . ioeGetErrorString
    result = httpDownloader request' `catchIOError` errorHandler `catch` statusExceptionHandler

describeHttpException :: HttpException -> T.Text
describeHttpException (StatusCodeException status _ _) =
    safeDecodeUtf8 . statusMessage $ status
describeHttpException (InvalidUrlException a b) = T.pack $ a ++ b
describeHttpException (TooManyRedirects _) = "Too many redirects"
describeHttpException (UnparseableRedirect _) = "Unparseable redirect"
describeHttpException (HttpParserException s) = T.pack s
describeHttpException (FailedConnectionException s _) = T.pack s
describeHttpException (FailedConnectionException2 s _ _ _) = T.pack s
describeHttpException (InvalidStatusLine bs) = safeDecodeUtf8 bs
describeHttpException (InvalidHeader bs) = safeDecodeUtf8 bs
describeHttpException (ProxyConnectException bs _ _) = safeDecodeUtf8 bs
describeHttpException (InvalidDestinationHost bs) = safeDecodeUtf8 bs

describeHttpException e = T.pack . show $ e

statusExceptionHandler :: HttpException -> IO (Either T.Text T.Text)
statusExceptionHandler = return . Left . describeHttpException

httpDownloader :: Request -> IO (Either T.Text T.Text)
httpDownloader request = do
    let tlsManagerSettings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- newManager tlsManagerSettings
    response <- httpLbs request manager
    let status = responseStatus response
    return $ if status == ok200
       then tryDecodeUtf8 . toStrict . responseBody $ response
       else Left . safeDecodeUtf8 . statusMessage $ status

