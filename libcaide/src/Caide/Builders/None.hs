module Caide.Builders.None (
      builder
) where

import Control.Monad.State (liftIO)

import Caide.Types

builder :: Builder
builder _ = do
    liftIO $ putStrLn "No builder configured. Continuing in assumption that tests have been run by an external tool"
    return TestsNotRun

