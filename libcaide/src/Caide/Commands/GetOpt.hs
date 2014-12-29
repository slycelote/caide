module Caide.Commands.GetOpt (
      cmd
    , cmdInternal
    , cmdProblem
) where

import Filesystem (isFile)

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
getIntOpt _ _ = return . Just $ usage cmdInternal


cmdProblem :: CommandHandler
cmdProblem = CommandHandler
    { command = "probgetopt"
    , description = "(Internal) Print problem option"
    , usage = "caide probgetopt problem section key"
    , action = getProbOpt
    }

getProbOpt :: CaideEnvironment -> [String] -> IO (Maybe String)
getProbOpt env [problem, section, key] = do
    let problemConfigFile = getProblemConfigFile env problem
    problemExists <- isFile problemConfigFile
    if problemExists
       then do
            problemConf <- readProblemConfig problemConfigFile
            putStrLn $ getProblemOption problemConf section key
            -- FIXME error if no such option
            return Nothing
       else return . Just $ "Problem " ++ problem ++ " doesn't exist"

getProbOpt _ _ = return . Just $ usage cmdProblem

