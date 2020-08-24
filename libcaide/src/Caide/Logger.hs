module Caide.Logger(
      logInfo
    , logWarn
    , logError
    , logDebug
) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Text (Text)
import qualified Data.Text.IO.Util as T

import Caide.Types (CaideIO, Verbosity(Debug), caideVerbosity)


logInfo :: MonadIO m => Text -> m ()
logInfo message = liftIO $ T.putStrLn message

logWarn :: MonadIO m => Text -> m ()
logWarn = logInfo

logError :: MonadIO m => Text -> m ()
logError = logInfo

logDebug :: Text -> CaideIO ()
logDebug message = do
    v <- caideVerbosity
    when (v >= Debug) $
        logInfo message

