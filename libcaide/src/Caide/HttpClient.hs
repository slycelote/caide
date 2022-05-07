{-# LANGUAGE CPP, NamedFieldPuns, OverloadedStrings #-}
module Caide.HttpClient(
      Client
    , ResponseLBS
    , newClient
    , sendRequest
    , get
    , post
    , Middleware
    , middleware
    , addHeaders
    , setTimeoutMs
    , setRedirectCount
    , module Network.HTTP.Client
) where

import qualified Control.Exception.Extended as Exc
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Time.Clock (DiffTime, diffTimeToPicoseconds)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Error (catchIOError)

import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..), Request,
    host, method, redirectCount, requestHeaders, requestBody,
    RequestBody(RequestBodyLBS),
    getUri, requestFromURI, setRequestCheckStatus,
    httpLbs,
    Response, responseStatus, responseBody, responseTimeout, responseTimeoutMicro, )

#ifdef VERSION_http_client_openssl
import Network.HTTP.Client.OpenSSL (newOpenSSLManager, withOpenSSL)
#else
import Network.Connection (TLSSettings(TLSSettingsSimple))
import Network.HTTP.Client.TLS (mkManagerSettings, newManager)
#endif
import Network.HTTP.Types.Header (RequestHeaders)
import Network.HTTP.Types.Status (statusCode, statusMessage)

import Network.URI (URI)



type ResponseLBS = Response LBS.ByteString

newtype Client = Client { send :: Request -> IO ResponseLBS }

type Middleware = Client -> Client


-- | Convenient middleware constructor that wraps and unwraps the newtype.
middleware :: ((Request -> IO ResponseLBS) -> Request -> IO ResponseLBS)
           -> Middleware
middleware impl Client{send} = Client $ impl send


-- | Send the request, catch IO exceptions and return them as Left.
sendRequest :: Client -> Request -> IO (Either Text LBS.ByteString)
sendRequest Client{send} request = catchExceptions (getUri request) $
    request & setRequestCheckStatus & send <&> responseBody <&> Right


get :: Client -> URI -> IO (Either Text LBS.ByteString)
get client uri = catchExceptions uri $
    sendRequest client =<< requestFromURI uri

post :: Client -> URI -> LBS.ByteString -> RequestHeaders -> IO (Either Text LBS.ByteString)
post client uri body headers = catchExceptions uri $ do
    request <- requestFromURI uri
    sendRequest client $ request
        { method = "POST"
        , requestBody = RequestBodyLBS body
        , requestHeaders = headers
        }


newClient :: IO Client
newClient = do
#ifdef VERSION_http_client_openssl
    manager <- withOpenSSL newOpenSSLManager
#else
    let tlsManagerSettings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- newManager tlsManagerSettings
#endif
    return $ Client $ \request -> httpLbs request manager


wrap :: (Request -> Request) -> (Client -> Client)
wrap modify Client{send} = Client $ send . modify

addHeaders :: RequestHeaders -> Middleware
addHeaders headers = wrap $ \request -> request{requestHeaders = requestHeaders request ++ headers}

setTimeoutMs :: DiffTime -> Middleware
setTimeoutMs timeout = wrap $ \request ->
    request{responseTimeout = responseTimeoutMicro (fromInteger $ diffTimeToPicoseconds timeout `div` 1000000)}

setRedirectCount :: Int -> Middleware
setRedirectCount n = wrap $ \request -> request{redirectCount = n}



catchExceptions :: URI -> IO (Either Text a) -> IO (Either Text a)
catchExceptions uri action = action `catchIOError` (
    \ioEx -> ioEx & show & T.pack & Left & return) `Exc.catch` (
    \httpEx -> return . Left $
        "HTTP error for URL '" <> T.pack (show uri) <> "'. " <> describeHttpException httpEx)

describeHttpException :: HttpException -> Text
describeHttpException (InvalidUrlException url reason) = "URL '" <> T.pack url <> "' is invalid: " <> T.pack reason
describeHttpException (HttpExceptionRequest _ content) = describeExceptionContent content

describeExceptionContent :: HttpExceptionContent -> Text
describeExceptionContent (StatusCodeException response _) =
    "HTTP status " <> T.pack (show $ statusCode status) <> ": " <> T.pack (show $ statusMessage status)
  where
    status = responseStatus response

describeExceptionContent (TooManyRedirects _) = "Too many redirects"
describeExceptionContent e = T.pack $ show e

