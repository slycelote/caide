module Caide.Commands.BuildScaffold (
      cmd
    , generateScaffoldSolution
) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Maybe (mapMaybe)

import Caide.Types

import Filesystem.Path.CurrentOS (decodeString, (</>))
import Caide.Registry (findLanguage, findFeature)
import Caide.Configuration (getActiveProblem, getProblemConfigFile, readProblemConfig,
                            saveProblemConfig, setProblemOption, getFeatures)

import Caide.Commands.Make (make)

cmd :: CommandHandler
cmd = CommandHandler
    { command = "lang"
    , description = "Generate solution scaffold"
    , usage = "caide lang <language>"
    , action = generateScaffoldSolution
    }

generateScaffoldSolution :: CaideEnvironment -> [String] -> IO ()
generateScaffoldSolution env [lang] = case findLanguage lang of
    Nothing -> putStrLn $ "Unknown or unsupported language: " ++ lang
    Just language -> do
        problem <- getActiveProblem env
        if null problem
            then putStrLn "No active problem. Generate one with `caide problem`"
            else do
                let caideRoot = getRootDirectory env
                    problemDir = caideRoot </> decodeString problem
                    problemConfigFile = getProblemConfigFile env problem
                generateScaffold language env problemDir
                problemConf <- readProblemConfig problemConfigFile
                saveProblemConfig (setProblemOption problemConf "problem" "language" lang) problemConfigFile
                features <- mapMaybe findFeature <$> getFeatures env
                forM_ features $ \feature -> onProblemCodeCreated feature env problem

                make env []

generateScaffoldSolution _ _ = putStrLn $ "Usage " ++ usage cmd

