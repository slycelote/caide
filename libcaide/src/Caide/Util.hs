{-# LANGUAGE OverloadedStrings #-}
{- | Common utilities
-}
module Caide.Util(
      downloadDocument
    , mapWithLimitedThreads
    , pathToText
    , tshow
    , listDir
    , copyFileToDir
    , copyTreeToDir
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Filesystem (copyFile, listDirectory, isFile, isDirectory, createDirectory)
import qualified Filesystem.Path as F
import Filesystem.Path (basename, filename, (</>))
import Filesystem.Path.CurrentOS (toText)
import Network.Browser (browse, request, setAllowRedirects, setMaxErrorRetries, setOutHandler)
import Network.HTTP (mkRequest, RequestMethod(GET), rspBody, rspCode, rspReason)
import Network.URI (parseURI, uriScheme, URI)
import System.IO.Error (catchIOError, ioeGetErrorString)

import Caide.Types (URL)


{- | Download a URL. Return (Left errorMessage) in case of an error,
     (Right doc) in case of success.
-}
downloadDocument :: URL -> IO (Either T.Text T.Text)
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
    case rspCode rsp of
        (5,a,b) -> return . Left .  T.pack $ show (500 + 10*a + b) ++ " " ++ rspReason rsp
        _       -> return . Right . T.pack $ rspBody rsp

-- TODO a more efficient algorithm
mapWithLimitedThreads :: Int -> (a -> IO b) -> [a] -> IO [b]
mapWithLimitedThreads _ _ [] = return []
mapWithLimitedThreads numThreads f tasks = do
    let (firstChunk, rest) = splitAt numThreads tasks
    firstRes <- mapConcurrently f firstChunk
    restRes  <- mapWithLimitedThreads numThreads f rest
    return $ firstRes ++ restRes

tshow :: Show a => a -> T.Text
tshow = T.pack . show

pathToText :: F.FilePath -> T.Text
pathToText path = case toText path of
    Left  s -> s
    Right s -> s

-- | Returns (file list, directory list)
listDir :: F.FilePath -> IO ([F.FilePath], [F.FilePath])
listDir dir = do
    filesAndDirs <- listDirectory dir
    thisIsFile <- mapM isFile filesAndDirs
    thisIsDir <- mapM isDirectory filesAndDirs
    let files = map fst . filter snd $ zip filesAndDirs thisIsFile
        dirs  = map fst . filter snd $ zip filesAndDirs thisIsDir
    return (files, dirs)

copyFileToDir :: F.FilePath -> F.FilePath -> IO ()
copyFileToDir srcFile dstDir = copyFile srcFile dstFile
    where dstFile = dstDir </> filename srcFile

copyTreeToDir :: F.FilePath -> F.FilePath -> IO ()
copyTreeToDir srcTree dstDir = do
    let targetDir = dstDir </> basename srcTree
    createDirectory True targetDir
    (files, dirs) <- listDir srcTree
    forM_ files $ \file -> copyFileToDir file targetDir
    forM_ dirs $ \dir -> copyTreeToDir dir targetDir

