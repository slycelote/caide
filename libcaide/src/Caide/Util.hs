{- | Common utilities
-}
module Caide.Util(
      downloadDocument
    , getProblemID
    , forceEither
    , copyFileToDir
    , splitString
) where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import Filesystem (copyFile)
import qualified Filesystem.Path as F
import Filesystem.Path (basename, filename, (</>))
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
getProblemID problemDir = encodeString . basename $ problemDir

forceEither :: Either a c -> c
forceEither = either (error "Left in forceEither") id

copyFileToDir :: F.FilePath -> F.FilePath -> IO ()
copyFileToDir srcFile dstDir = copyFile srcFile dstFile
    where dstFile = dstDir </> filename srcFile

splitString :: String -> String -> [String]
splitString separators s = reverse (go s []) where
    go [] parts = parts
    go str parts = let (_, rest)     = span (`elem` separators) str
                       (word, rest') = break (`elem` separators) rest
                   in go rest' (if null word then parts else word:parts)
