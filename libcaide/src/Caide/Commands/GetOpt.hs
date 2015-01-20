{-# LANGUAGE OverloadedStrings #-}
module Caide.Commands.GetOpt (
      cmd
    , cmdState
    , cmdProblem
    , cmdProblemState
) where

import Control.Monad.State (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Caide.Configuration (readProblemConfig, readProblemState, readCaideConf, readCaideState)
import Caide.Types

cmd :: CommandHandler
cmd = CommandHandler
    { command = "getopt"
    , description = "(Internal) Print option"
    , usage = "caide getopt section key"
    , action = getOpt
    }


getOpt :: [T.Text] -> CaideIO ()
getOpt [section, key] = do
    h <- readCaideConf
    val <- getProp h (T.unpack section) (T.unpack key)
    liftIO $ T.putStrLn val
getOpt _ = throw $ usage cmd

cmdState :: CommandHandler
cmdState = CommandHandler
    { command = "getstate"
    , description = "(Internal) Print option"
    , usage = "caide getstate section key"
    , action = getState
    }

getState :: [T.Text] -> CaideIO ()
getState [section, key] = do
    h <- readCaideState
    val <- getProp h (T.unpack section) (T.unpack key)
    liftIO $ T.putStrLn val
getState _ = throw $ usage cmdState


cmdProblem :: CommandHandler
cmdProblem = CommandHandler
    { command = "probgetopt"
    , description = "(Internal) Print problem option"
    , usage = "caide probgetopt problem section key"
    , action = getProbOpt
    }

getProbOpt :: [T.Text] -> CaideIO ()
getProbOpt [problem, section, key] = do
    h <- readProblemConfig problem
    val <- getProp h (T.unpack section) (T.unpack key)
    liftIO $ T.putStrLn val
getProbOpt _ = throw $ usage cmdProblem

cmdProblemState :: CommandHandler
cmdProblemState = CommandHandler
    { command = "probgetstate"
    , description = "(Internal) Print problem option"
    , usage = "caide probgetstate problem section key"
    , action = getProbState
    }

getProbState :: [T.Text] -> CaideIO ()
getProbState [problem, section, key] = do
    h <- readProblemState problem
    val <- getProp h (T.unpack section) (T.unpack key)
    liftIO $ T.putStrLn val
getProbState _ = throw $ usage cmdProblemState

