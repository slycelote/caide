module Caide.Commands.Checkout (
      cmd
) where

import Filesystem (isDirectory)
import Filesystem.Path.CurrentOS (decodeString, (</>))

import Caide.Configuration (setActiveProblem)
import Caide.Types

cmd :: CommandHandler
cmd = CommandHandler
    { command = "checkout"
    , description = "Change active problem"
    , usage = "caide checkout <problem id>"
    , action = checkoutProblem
    }

checkoutProblem :: CaideEnvironment -> [String] -> IO ()
checkoutProblem env [probId] = do
    let caideRoot = getRootDirectory env
        problemDir = caideRoot </> decodeString probId
    problemExists <- isDirectory problemDir
    if problemExists
        then do
            setActiveProblem env probId
            putStrLn $ "Checked out problem " ++ probId
        else putStrLn $ "Problem " ++ probId ++ " doesn't exist"

checkoutProblem _ _ = putStrLn $ "Usage: " ++ usage cmd
