{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Util(
      downloadDocument
) where

import Control.Exception (catch)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Network.HTTP.Client (HttpException(..), httpLbs, newManager, parseUrlThrow,
                            responseStatus, responseBody, responseTimeout, responseTimeoutMicro, requestHeaders, Request)
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
    mbRequest = parseUrlThrow $ T.unpack url
    Just request = mbRequest
    request' = request {
        responseTimeout = responseTimeoutMicro $ 15*1000*1000, -- 15 seconds
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

