module Caide.Builders.None (
      builder
) where

import Control.Monad.IO.Class (liftIO)

import Caide.Types.Builder (Builder, BuilderResult(NoEvalTests))

builder :: Builder
builder _ = do
    liftIO $ putStrLn "No builder configured. Continuing in assumption that tests have been run by an external tool"
    return NoEvalTests

