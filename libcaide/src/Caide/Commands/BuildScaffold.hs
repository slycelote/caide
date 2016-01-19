{-# LANGUAGE CPP, OverloadedStrings #-}
module Caide.Commands.BuildScaffold(
      generateScaffoldSolution
) where

#ifndef AMP
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM_)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T

import Caide.Types
import Caide.Registry (findLanguage, findFeature)
import Caide.Configuration (getActiveProblem, readProblemState, getFeatures)
import Caide.Util (withLock)


generateScaffoldSolution :: T.Text -> CaideIO ()
generateScaffoldSolution lang = case findLanguage lang of
    Nothing      -> throw . T.concat $ ["Unknown or unsupported language: ", lang]
    Just ([], _) -> throw "Unexpected language"
    Just (canonicalLanguageName:_, language) -> withLock $ do
        problem <- getActiveProblem

        generateScaffold language problem

        hProblem <- readProblemState problem
        setProp hProblem "problem" "language" canonicalLanguageName

        features <- mapMaybe findFeature <$> getFeatures
        forM_ features (`onProblemCodeCreated` problem)

