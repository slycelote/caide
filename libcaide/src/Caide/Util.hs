{-# LANGUAGE OverloadedStrings #-}
{- | Common utilities
-}
module Caide.Util(
      downloadDocument
    , mapWithLimitedThreads
    , runHtmlParser
    , tshow
    , readTextFile'
    , withLock
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.State (liftIO)
import qualified Data.Text as T
import qualified Filesystem.Path as F
import Filesystem.Path ((</>))
import Filesystem.Path.CurrentOS (encodeString)

import System.FileLock (SharedExclusive(Exclusive), tryLockFile, unlockFile)

import Filesystem.Util (pathToText, readTextFile)
import Network.HTTP.Util (downloadDocument)
import Caide.Configuration (orDefault)
import Caide.Settings (useFileLock)
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
    hTemp <- getTemporaryConf
    useLock <- useFileLock <$> caideSettings
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

