module Caide.Commands.Checkout (
      cmd
) where

import Filesystem (isDirectory)
import Filesystem.Path.CurrentOS (decodeString, (</>))
import qualified Filesystem.Path as F

import Caide.Configuration (readRootConf, saveRootConf, setActiveProblem)
import Caide.Types

cmd :: CommandHandler
cmd = CommandHandler
    { command = "checkout"
    , description = "Change active problem"
    , usage = "caide checkout <problem id>"
    , action = checkoutProblem
    }

checkoutProblem :: F.FilePath -> [String] -> IO ()
checkoutProblem caideRoot [probId] = do
    let problemDir = caideRoot </> decodeString probId
    problemExists <- isDirectory problemDir
    if problemExists
        then do
            conf <- readRootConf caideRoot
            saveRootConf caideRoot $ setActiveProblem conf probId
            putStrLn $ "Checked out problem " ++ probId
        else putStrLn $ "Problem " ++ probId ++ " doesn't exist"

checkoutProblem _ _ = putStrLn $ "Usage: " ++ usage cmd
