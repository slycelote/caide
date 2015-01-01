module Caide.Commands.GetOpt (
      cmd
    , cmdState
    , cmdProblem
    , cmdProblemState
) where

import Control.Monad.State (liftIO)

import Caide.Configuration (readProblemConfig, readProblemState, readCaideConf, readCaideState)
import Caide.Types

cmd :: CommandHandler
cmd = CommandHandler
    { command = "getopt"
    , description = "(Internal) Print option"
    , usage = "caide getopt section key"
    , action = getOpt
    }


getOpt :: [String] -> CaideIO ()
getOpt [section, key] = do
    h <- readCaideConf
    val <- getProp h section key
    liftIO $ putStrLn val
getOpt _ = throw $ usage cmd

cmdState :: CommandHandler
cmdState = CommandHandler
    { command = "getstate"
    , description = "(Internal) Print option"
    , usage = "caide getstate section key"
    , action = getState
    }

getState :: [String] -> CaideIO ()
getState [section, key] = do
    h <- readCaideState
    val <- getProp h section key
    liftIO $ putStrLn val
getState _ = throw $ usage cmdState


cmdProblem :: CommandHandler
cmdProblem = CommandHandler
    { command = "probgetopt"
    , description = "(Internal) Print problem option"
    , usage = "caide probgetopt problem section key"
    , action = getProbOpt
    }

getProbOpt :: [String] -> CaideIO ()
getProbOpt [problem, section, key] = do
    h <- readProblemConfig problem
    val <- getProp h section key
    liftIO $ putStrLn val
getProbOpt _ = throw $ usage cmdProblem

cmdProblemState :: CommandHandler
cmdProblemState = CommandHandler
    { command = "probgetstate"
    , description = "(Internal) Print problem option"
    , usage = "caide probgetstate problem section key"
    , action = getProbState
    }

getProbState :: [String] -> CaideIO ()
getProbState [problem, section, key] = do
    h <- readProblemState problem
    val <- getProp h section key
    liftIO $ putStrLn val
getProbState _ = throw $ usage cmdProblemState

