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
    , withLock
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.State (liftIO)
import qualified Data.Text as T
import Filesystem (copyFile, listDirectory, isFile, isDirectory, createDirectory)
import qualified Filesystem.Path as F
import Filesystem.Path (basename, filename, (</>))
import Filesystem.Path.CurrentOS (encodeString, toText)

import System.FileLock (SharedExclusive(Exclusive), tryLockFile, unlockFile)

import Filesystem.Util (readTextFile)
import Network.HTTP.Util (downloadDocument)
import Caide.Configuration (orDefault, readCaideConf)
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

finally :: (Monad m) => CaideM m a -> CaideM m () -> CaideM m a
finally action finalizer = do
    ret <- catchError action $ \e -> do
        finalizer
        throwError e
    finalizer
    return ret

withLock :: CaideIO () -> CaideIO ()
withLock action = do
    h <- readCaideConf
    hTemp <- getTemporaryConf
    useLock <- getProp h "core" "use_lock" `orDefault` True
    haveLock <- getProp hTemp "DEFAULT" "have_lock" `orDefault` False
    if not useLock || haveLock
    then action
    else do
        root <- caideRoot
        let lockPath = root </> ".caide" </> "lock"
        mbLock <- liftIO $ tryLockFile (encodeString lockPath) Exclusive
        case mbLock of
            Nothing -> throw $ T.concat ["Couldn't lock file ", pathToText lockPath,
                        ". Make sure that another instance of caide is not running."]
            Just lock -> do
                setProp hTemp "DEFAULT" "have_lock" True
                finally action $ do
                    liftIO $ unlockFile lock
                    setProp hTemp "DEFAULT" "have_lock" False

