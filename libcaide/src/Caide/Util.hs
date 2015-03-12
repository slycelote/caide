{-# LANGUAGE OverloadedStrings #-}
{- | Common utilities
-}
module Caide.Util(
      downloadDocument
    , mapWithLimitedThreads
    , runHtmlParser
    , pathToText
    , tshow
    , listDir
    , copyFileToDir
    , copyTreeToDir
    , readTextFile'
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_)
import Control.Monad.State (liftIO)
import qualified Data.Text as T
import Filesystem (copyFile, listDirectory, isFile, isDirectory, createDirectory)
import qualified Filesystem.Path as F
import Filesystem.Path (basename, filename, (</>))
import Filesystem.Path.CurrentOS (toText)

import Filesystem.Util (readTextFile)
import Network.HTTP.Util (downloadDocument)
import Caide.Types


runHtmlParser :: (T.Text -> Either T.Text (Problem, [TestCase]))
              -> URL -> IO (Either T.Text (Problem, [TestCase]))
runHtmlParser parser url = do
    doc <- downloadDocument url
    case doc of
        Left err   -> return $ Left err
        Right cont -> return $ parser cont


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


readTextFile' :: F.FilePath -> CaideIO T.Text
readTextFile' filePath = do
    contents <- liftIO $ readTextFile filePath
    case contents of
        Left err   -> throw err
        Right cont -> return cont


