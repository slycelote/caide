{-# LANGUAGE OverloadedStrings #-}
{- | Common utilities
-}
module Caide.Util(
      newDefaultHttpClient
    , mapWithLimitedThreads
    , tshow
    , readTextFile'
    , withLock
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Extended (catchError, throwError, liftIO)
import Data.IORef (readIORef, writeIORef)
import qualified Data.Text as T
import qualified Filesystem.Path as F
import Filesystem.Path ((</>))
import Filesystem.Path.CurrentOS (encodeString)

import System.FileLock (SharedExclusive(Exclusive), tryLockFile, unlockFile)

import Network.HTTP.Types.Header (hAccept, hAcceptEncoding, hUserAgent)

import Filesystem.Util (pathToText, readTextFile)
import qualified Caide.CodeforcesCookie as CodeforcesCookie
import qualified Caide.HttpClient as Http
import Caide.Monad (CaideIO, CaideM, throw, caideHoldingLock, caideRoot)


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

{- HLINT ignore newDefaultHttpClient "Redundant $" -}
newDefaultHttpClient :: IO Http.Client
newDefaultHttpClient = do
    client <- Http.newClient
    cfMiddleware <- CodeforcesCookie.newHttpMiddleware
    let defaultHeaders =
            [ (hAcceptEncoding, "") -- omit this header
            , (hUserAgent, "Mozilla/5.0 (Windows NT 10.0; rv:142.0) Gecko/20100101 Firefox/142.0")
            , (hAccept, "*/*")
            ]
    return $
        Http.throwOnHttpErrors $
        Http.setTimeout 5.0 $
        Http.addHeaders defaultHeaders $
        cfMiddleware $
        -- Http.logToFile "/tmp/requests.log" $
        client

readTextFile' :: F.FilePath -> CaideIO T.Text
readTextFile' filePath = do
    contents <- liftIO $ readTextFile filePath
    case contents of
        Left err   -> throw err
        Right cont -> return cont

-- FIXME: This doesn't handle IO exceptions
finally :: (Monad m) => CaideM m a -> CaideM m () -> CaideM m a
finally action finalizer = do
    ret <- catchError action $ \e -> do
        finalizer
        throwError e
    finalizer
    return ret

withLock :: CaideIO () -> CaideIO ()
withLock action = do
    mbHoldingLock <- caideHoldingLock
    holdingLock <- maybe (pure False) (liftIO . readIORef) mbHoldingLock
    case mbHoldingLock of
        Just holdingLockRef | holdingLock -> do
            root <- caideRoot
            let lockPath = root </> ".caide" </> "lock"
            mbLock <- liftIO $ tryLockFile (encodeString lockPath) Exclusive
            case mbLock of
                -- TODO: Retry
                Nothing -> throw $ T.concat ["Couldn't lock file ", pathToText lockPath,
                            ". Make sure that another instance of caide is not running."]
                Just lock -> do
                    liftIO $ writeIORef holdingLockRef True
                    finally action $ liftIO $ do
                        unlockFile lock
                        writeIORef holdingLockRef False
        _ -> action
