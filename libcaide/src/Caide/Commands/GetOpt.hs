module Caide.Commands.GetOpt (
      cmd
    , cmdInternal
    , cmdProblem
) where

import Caide.Configuration (readProblemConfig, getProblemConfigFile, getProblemOption)
import Caide.Types

cmd :: CommandHandler
cmd = CommandHandler
    { command = "getopt"
    , description = "(Internal) Print option"
    , usage = "caide getopt section key"
    , action = getOpt
    }

getOpt :: CaideEnvironment -> [String] -> IO (Maybe String)
getOpt env [section, key] = getUserOption env section key >>= putStrLn >> return Nothing
getOpt _ _ = return . Just $ usage cmd


cmdInternal :: CommandHandler
cmdInternal = CommandHandler
    { command = "intgetopt"
    , description = "(Internal) Print option"
    , usage = "caide intgetopt section key"
    , action = getIntOpt
    }

getIntOpt :: CaideEnvironment -> [String] -> IO (Maybe String)
getIntOpt env [section, key] = getInternalOption env section key >>= putStrLn >> return Nothing
getIntOpt _ _ = return . Just $ usage cmd


cmdProblem :: CommandHandler
cmdProblem = CommandHandler
    { command = "probgetopt"
    , description = "(Internal) Print problem option"
    , usage = "caide probgetopt problem section key"
    , action = getProbOpt
    }

getProbOpt :: CaideEnvironment -> [String] -> IO (Maybe String)
getProbOpt env [problem, section, key] = do
    problemConf <- readProblemConfig $ getProblemConfigFile env problem
    putStrLn $ getProblemOption problemConf section key
    -- FIXME error if no such option
    return Nothing

getProbOpt _ _ = return . Just $ usage cmd

