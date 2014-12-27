module Caide.Commands.Checkout (
      cmd
) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Maybe (mapMaybe)

import Filesystem (isDirectory)
import Filesystem.Path.CurrentOS (decodeString, (</>))

import Caide.Configuration (setActiveProblem, getFeatures)
import Caide.Registry (findFeature)
import Caide.Types

cmd :: CommandHandler
cmd = CommandHandler
    { command = "checkout"
    , description = "Change active problem"
    , usage = "caide checkout <problem id>"
    , action = checkoutProblem
    }

checkoutProblem :: CaideEnvironment -> [String] -> IO (Maybe String)
checkoutProblem env [probId] = do
    let caideRoot  = getRootDirectory env
        problemDir = caideRoot </> decodeString probId
    problemExists <- isDirectory problemDir
    if problemExists
        then do
            setActiveProblem env probId
            putStrLn $ "Checked out problem " ++ probId
            features <- mapMaybe findFeature <$> getFeatures env
            forM_ features $ \feature -> onProblemCheckedOut feature env probId
            return Nothing
        else return . Just $ "Problem " ++ probId ++ " doesn't exist"

checkoutProblem _ _ = return . Just $ "Usage: " ++ usage cmd

