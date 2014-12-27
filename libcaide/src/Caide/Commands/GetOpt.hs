module Caide.Commands.GetOpt (
      cmd
    , cmdInternal
) where

import Caide.Types

cmd :: CommandHandler
cmd = CommandHandler
    { command = "getopt"
    , description = "(Internal) Print option"
    , usage = "caide getopt section key"
    , action = getOpt
    }

getOpt :: CaideEnvironment -> [String] -> IO ()
getOpt env [section, key] = getUserOption env section key >>= putStrLn
getOpt _ _ = putStrLn $ usage cmd


cmdInternal :: CommandHandler
cmdInternal = CommandHandler
    { command = "intgetopt"
    , description = "(Internal) Print option"
    , usage = "caide intgetopt section key"
    , action = getIntOpt
    }

getIntOpt :: CaideEnvironment -> [String] -> IO ()
getIntOpt env [section, key] = getInternalOption env section key >>= putStrLn
getIntOpt _ _ = putStrLn $ usage cmd

