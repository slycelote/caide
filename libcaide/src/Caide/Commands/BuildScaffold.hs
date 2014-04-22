module Caide.Commands.BuildScaffold (
      cmd
) where

import Caide.Types

import qualified Filesystem.Path as F
import Filesystem.Path.CurrentOS (decodeString, (</>))
import Caide.Registry (findLanguage)
import Caide.Configuration (readRootConf, getActiveProblem)

cmd :: CommandHandler
cmd = CommandHandler
    { command = "lang"
    , description = "Generate solution scaffold"
    , usage = "caide lang <language>"
    , action = generateScaffoldSolution
    }

generateScaffoldSolution :: F.FilePath -> [String] -> IO ()
generateScaffoldSolution caideRoot [lang] = case findLanguage lang of
    Nothing -> putStrLn $ "Unknown or unsupported language: " ++ lang
    Just language -> do
        conf <- readRootConf caideRoot
        let problem = getActiveProblem conf
        if null problem
            then putStrLn "No active problem. Generate one with `caide problem`"
            else language `generateScaffold` (caideRoot </> decodeString problem)
