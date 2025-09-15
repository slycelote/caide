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
    , setTimeout
    , setRedirectCount
    , logToFile
    , throwOnHttpErrors
    , module Network.HTTP.Client
) where

import qualified Control.Exception.Extended as Exc
import Control.Monad.Extended (MonadIO, liftIO)
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
import Network.HTTP.Client.TLS (newTlsManager)
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
    request & send <&> responseBody <&> Right


get :: MonadIO m => Client -> URI -> m (Either Text LBS.ByteString)
get client uri = liftIO $ catchExceptions uri $
    sendRequest client =<< requestFromURI uri

post :: MonadIO m => Client -> URI -> LBS.ByteString -> RequestHeaders -> m (Either Text LBS.ByteString)
post client uri body headers = liftIO $ catchExceptions uri $ do
    request <- requestFromURI uri
    sendRequest client $ request
        { method = "POST"
        , requestBody = RequestBodyLBS body
        , requestHeaders = headers
        }


newClient :: MonadIO m => m Client
newClient = liftIO $ do
#ifdef VERSION_http_client_openssl
    manager <- withOpenSSL newOpenSSLManager
#else
    manager <- newTlsManager
#endif
    return $ Client $ \request -> httpLbs request manager


wrap :: (Request -> Request) -> (Client -> Client)
wrap modify = middleware $ \send request -> send (modify request)

addHeaders :: RequestHeaders -> Middleware
addHeaders headers = wrap $ \request -> request{requestHeaders = requestHeaders request ++ headers}

setTimeout :: DiffTime -> Middleware
setTimeout timeout = wrap $ \request ->
    request{responseTimeout = responseTimeoutMicro (fromInteger $ diffTimeToPicoseconds timeout `div` 1000000)}

setRedirectCount :: Int -> Middleware
setRedirectCount n = wrap $ \request -> request{redirectCount = n}

throwOnHttpErrors :: Middleware
throwOnHttpErrors = wrap setRequestCheckStatus

logToFile :: FilePath -> Middleware
logToFile filePath = middleware $ \send request -> do
    appendFile filePath $ show request ++ "\n"
    res <- Exc.tryWithContext $ send request
    appendFile filePath $ show res ++ "\n"
    case res of
        Left e -> Exc.rethrowIO (e :: Exc.ExceptionWithContext HttpException)
        Right resp -> pure resp


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

