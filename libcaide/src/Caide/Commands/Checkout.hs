module Caide.Commands.Checkout (
      cmd
) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.State (liftIO)
import Data.Maybe (mapMaybe)

import Filesystem (isDirectory)
import Filesystem.Path.CurrentOS (decodeString, (</>))

import Caide.Configuration (setActiveProblem, getActiveProblem, getFeatures)
import Caide.Registry (findFeature)
import Caide.Types

cmd :: CommandHandler
cmd = CommandHandler
    { command = "checkout"
    , description = "Change active problem"
    , usage = "caide checkout <problem id>"
    , action = checkoutProblem
    }

checkoutProblem :: [String] -> CaideIO ()
checkoutProblem [probId] = do
    root <- caideRoot
    let problemDir = root </> decodeString probId
    problemExists <- liftIO $ isDirectory problemDir

    if problemExists
        then do
            currentProbId <- getActiveProblem
            if currentProbId == probId
                then liftIO $ putStrLn $ probId ++ ": already checked out"
                else do
                    setActiveProblem probId
                    liftIO $ putStrLn $ "Checked out problem " ++ probId
                    features <- mapMaybe findFeature <$> getFeatures
                    forM_ features $ \feature -> onProblemCheckedOut feature probId
        else throw $ "Problem " ++ probId ++ " doesn't exist"

checkoutProblem _ = throw $ "Usage: " ++ usage cmd

