module Caide.Builders.None (
      builder
) where

import Caide.Types

builder :: Builder
builder _ _ = do
    putStrLn "No builder configured. Continuing in assumption that tests have been run by an external tool"
    return True
