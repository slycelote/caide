{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Util(
      downloadDocument
) where

import Data.Maybe (isNothing)
import qualified Data.Text as T
import Network.Browser (browse, request, setAllowRedirects, setMaxErrorRetries, setOutHandler)
import Network.HTTP (mkRequest, RequestMethod(GET), rspBody, rspCode, rspReason)
import Network.URI (parseURI, uriScheme, URI)
import System.IO.Error (catchIOError, ioeGetErrorString)

import Data.Text.Encoding.Util (tryDecodeUtf8)

{- | Download an HTML document. Return (Left errorMessage) in case of an error,
     (Right doc) in case of success.
-}
downloadDocument :: T.Text -> IO (Either T.Text T.Text)
downloadDocument url
    | isNothing maybeUri        = mkLiftedError "URL not supported"
    | "http:" == uriScheme uri  = result
    | otherwise                 = mkLiftedError "URL not supported"
  where
    maybeUri = parseURI $ T.unpack url
    Just uri = maybeUri
    mkLiftedError = return . Left
    errorHandler = mkLiftedError . T.pack . ioeGetErrorString
    result = httpDownloader uri `catchIOError` errorHandler


httpDownloader :: URI -> IO (Either T.Text T.Text)
httpDownloader uri = do
    (_, rsp) <- browse $ do
        -- setErrHandler $ const $ return ()
        setOutHandler $ const $ return ()
        setAllowRedirects True
        setMaxErrorRetries $ Just 5
        request $ mkRequest GET uri
    let getResponseCode (a,b,c) = 100*a + 10*b + c
    return $ case getResponseCode (rspCode rsp) of
        200 -> tryDecodeUtf8 $ rspBody rsp
        err -> Left .  T.pack $ show err ++ " " ++ rspReason rsp

