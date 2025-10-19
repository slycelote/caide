{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Caide.Logger(
      configure
    , ColorOutput(..)
    , Verbosity(..)
    , LogSettings(..)
    , logInfo
    , logSuccess
    , logWarn
    , logError
    , logDebug
    , autoDeterminedColorCapability
) where

import Control.Monad.Extended (when, MonadIO, liftIO)
import Data.IORef (IORef, newIORef, atomicWriteIORef, atomicModifyIORef')
import Data.Text (Text)
import qualified Data.Text.IO.Utf8 as T
import qualified System.Info as System
import qualified System.IO as System
import System.IO.Unsafe (unsafePerformIO)

import System.Console.ANSI

data Verbosity = Info | Debug
    deriving (Show, Enum, Ord, Eq, Bounded)

data ColorOutput = ColorAuto | ColorYes | ColorNo
    deriving (Show)

data LogSettings = LogSettings
    { color     :: !Bool
    , verbosity :: !Verbosity
    } deriving (Show)

-- TODO: improve support for color and Unicode symbols in Windows
autoDeterminedColorCapability :: IO Bool
autoDeterminedColorCapability = if System.os == "mingw32"
    then pure False
    else hNowSupportsANSI System.stdout

logSettings :: IORef LogSettings
{-# NOINLINE logSettings #-}
logSettings = unsafePerformIO $ newIORef $
    LogSettings{color=False, verbosity=Info}

configure :: LogSettings -> IO ()
configure settings = atomicWriteIORef logSettings settings

getSettings :: IO LogSettings
getSettings = atomicModifyIORef' logSettings $ \s -> (s, s)

logInfo :: MonadIO m => Text -> m ()
logInfo message = liftIO $ T.putStrLn message

logSuccess :: MonadIO m => Text -> m ()
logSuccess message = liftIO $ do
    LogSettings{color} <- getSettings
    when color $ do
        setSGR [SetColor Foreground Vivid Green]
        T.putStr "✔ "
        setSGR [Reset]
    T.putStrLn message

logWarn :: MonadIO m => Text -> m ()
logWarn message = liftIO $ do
    LogSettings{color} <- getSettings
    when color $ do
        setSGR [SetColor Foreground Vivid Yellow]
        T.putStr "‼ "
        setSGR [Reset]
    T.putStrLn message

logError :: MonadIO m => Text -> m ()
logError message = liftIO $ do
    LogSettings{color} <- getSettings
    when color $ do
        setSGR [SetColor Foreground Vivid Red]
        T.putStr "✗ "
        setSGR [Reset, SetConsoleIntensity BoldIntensity]
    T.putStrLn message
    when color $ setSGR [Reset]

logDebug :: MonadIO m => Text -> m ()
logDebug message = liftIO $ do
    LogSettings{verbosity} <- getSettings
    when (verbosity >= Debug) $ T.putStrLn message

