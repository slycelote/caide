{-# LANGUAGE OverloadedStrings #-}
{- | Common utilities
-}
module Caide.Util(
      downloadDocument
    , getProblemID
    , pathToText
    , tshow
    , forceEither
    , listDir
    , copyFileToDir
    , copyTreeToDir
    , splitString
    , trimString
) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Char (isSpace)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Filesystem (copyFile, listDirectory, isFile, isDirectory, createDirectory)
import qualified Filesystem.Path as F
import Filesystem.Path (basename, filename, (</>))
import Filesystem.Path.CurrentOS (toText)
import Network.Browser (browse, request, setAllowRedirects, setOutHandler)
import Network.HTTP (mkRequest, RequestMethod(GET), rspBody)
import Network.URI (parseURI, uriScheme, URI)
import System.IO.Error (catchIOError, ioeGetErrorString)

import Caide.Types (ProblemID, URL)


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
    result = (Right <$> httpDownloader uri) `catchIOError` errorHandler

httpDownloader :: URI -> IO T.Text
httpDownloader uri = do
    (_, rsp) <- browse $ do
        -- setErrHandler $ const $ return ()
        setOutHandler $ const $ return ()
        setAllowRedirects True
        request $ mkRequest GET uri
    return . T.pack $ rspBody rsp

tshow :: Show a => a -> T.Text
tshow = T.pack . show

pathToText :: F.FilePath -> T.Text
pathToText path = case toText path of
    Left  s -> s
    Right s -> s

getProblemID :: F.FilePath -> ProblemID
getProblemID = pathToText . basename

forceEither :: Either a c -> c
forceEither = either (error "Left in forceEither") id

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


splitString :: String -> String -> [String]
splitString separators s = reverse (go s []) where
    go [] parts = parts
    go str parts = let (_, rest)     = span (`elem` separators) str
                       (word, rest') = break (`elem` separators) rest
                   in go rest' $ if null word then parts else word:parts

trimString :: String -> String
trimString = reverse . dropWhile isSpace . reverse . dropWhile isSpace
