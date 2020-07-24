module Caide.Logger(
      logWarn
) where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Text (Text)
import qualified Data.Text.IO as T

logWarn :: MonadIO m => Text -> m ()
logWarn message = liftIO $ T.putStrLn message

