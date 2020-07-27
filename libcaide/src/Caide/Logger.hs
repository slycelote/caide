module Caide.Logger(
      logInfo
    , logWarn
    , logError
) where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Text (Text)
import qualified Data.Text.IO as T

logInfo :: MonadIO m => Text -> m ()
logInfo message = liftIO $ T.putStrLn message

logWarn :: MonadIO m => Text -> m ()
logWarn = logInfo

logError :: MonadIO m => Text -> m ()
logError = logInfo

