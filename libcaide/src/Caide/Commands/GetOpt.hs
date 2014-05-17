module Caide.Commands.GetOpt (
      cmd
) where

import Caide.Types

cmd :: CommandHandler
cmd = CommandHandler
    { command = "getopt"
    , description = "Print option"
    , usage = "caide getopt section key"
    , action = getOpt
    }

getOpt :: CaideEnvironment -> [String] -> IO ()
getOpt env [section, key] = getUserOption env section key >>= putStrLn
getOpt _ _ = putStrLn $ usage cmd
