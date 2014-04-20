{- | Common utilities
-}
module Caide.Util(
      downloadDocument
    , getProblemID
    , (~>)
) where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Filesystem.Path as F
import Filesystem.Path (dirname)
import Filesystem.Path.CurrentOS (encodeString)
import Network.HTTP
import Network.URI (parseURI)
import System.IO.Error (catchIOError, ioeGetErrorString)

import Caide.Types (ProblemID, URL)


{- | Download a URL. Return (Left errorMessage) in case of an error,
(Right doc) in case of success.
Based on code snippet from 'Real World Haskell'.
-}
-- TODO: retry download if anything wrong
downloadDocument :: URL -> IO (Either String T.Text)
downloadDocument url
    | T.pack "http" `T.isPrefixOf` url  =  result
    | otherwise = mkLiftedError "Not implemented"
  where
    mkLiftedError = return . Left
    result = downloader `catchIOError` (mkLiftedError . ioeGetErrorString)
    downloader = do
        let request = Request {rqURI = fromJust . parseURI $ T.unpack url,
                               rqMethod = GET,
                               rqHeaders = [],
                               rqBody = ""}
          
        resp <- simpleHTTP request
        case resp of
            Left x  -> mkLiftedError $ "Error connecting: " ++ show x
            Right r -> case rspCode r of
                (2,_,_) -> return . Right . T.pack $ rspBody r
                (3,_,_) -> -- An HTTP redirect
                    case findHeader HdrLocation r of
                        Nothing   -> mkLiftedError $ show r
                        -- FIXME: avoid infinite recursion
                        Just url' -> downloadDocument $ T.pack url'
                _ -> mkLiftedError $ show r

getProblemID :: F.FilePath -> ProblemID
getProblemID problemDir = encodeString . dirname $ problemDir

(~>) :: (a -> b) -> (b -> c) -> a -> c
(~>) = flip (.)
