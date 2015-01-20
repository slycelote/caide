{-# LANGUAGE OverloadedStrings #-}
module Caide.Commands.BuildScaffold (
      cmd
    , generateScaffoldSolution
) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Filesystem.Path.CurrentOS (fromText, (</>))

import Caide.Types
import Caide.Registry (findLanguage, findFeature)
import Caide.Configuration (getActiveProblem, readProblemState, getFeatures)


cmd :: CommandHandler
cmd = CommandHandler
    { command = "lang"
    , description = "Generate solution scaffold"
    , usage = "caide lang <language>"
    , action = generateScaffoldSolution
    }

generateScaffoldSolution :: [T.Text] -> CaideIO ()
generateScaffoldSolution [lang] = case findLanguage lang of
    Nothing -> throw . T.concat $ ["Unknown or unsupported language: ", lang]
    Just language -> do
        root <- caideRoot
        problem <- getActiveProblem

        let problemDir = root </> fromText problem
        generateScaffold language problemDir

        hProblem <- readProblemState problem
        setProp hProblem "problem" "language" lang

        features <- mapMaybe findFeature <$> getFeatures
        forM_ features (`onProblemCodeCreated` problem)

generateScaffoldSolution _ = throw . T.concat $ ["Usage ", usage cmd]

