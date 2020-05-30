{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Util(
      downloadDocument
) where

import Control.Exception (catch)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..), httpLbs, newManager, parseUrlThrow,
                            responseStatus, responseBody, responseTimeout, responseTimeoutMicro, requestHeaders, Request)
import Network.Connection (TLSSettings(TLSSettingsSimple))
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Types.Header (hAccept, hAcceptEncoding, hUserAgent)
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
            , (hUserAgent, "wget")
            , (hAccept, "*/*")
            ]
    }
    mkLiftedError = return . Left
    errorHandler = mkLiftedError . T.pack . ioeGetErrorString
    result = httpDownloader request' `catchIOError` errorHandler `catch` statusExceptionHandler url

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
    let tlsManagerSettings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- newManager tlsManagerSettings
    response <- httpLbs request manager
    let status = responseStatus response
    return $ if status == ok200
       then tryDecodeUtf8 . toStrict . responseBody $ response
       else Left . safeDecodeUtf8 . statusMessage $ status

