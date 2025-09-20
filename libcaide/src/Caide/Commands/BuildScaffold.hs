module Caide.Commands.BuildScaffold(
      generateScaffoldSolution
) where

import Control.Monad (forM_)
import Data.Maybe (mapMaybe)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import Caide.Monad (CaideIO, throw, caideSettings, onProblemCodeCreated)
import Caide.Types.ProgrammingLanguage (generateScaffold)
import Caide.Registry (findLanguage, findFeature)
import Caide.GlobalState (readGlobalState, activeProblem, noActiveProblemError)
import qualified Caide.GlobalTemplate as GlobalTemplate
import qualified Caide.Problem as Problem
import Caide.Settings (enabledFeatureNames)
import Caide.Util (withLock)


generateScaffoldSolution :: T.Text -> CaideIO ()
generateScaffoldSolution lang = withLock $ do
    settings <- caideSettings
    mbProblem <- activeProblem <$> readGlobalState
    problem <- maybe (throw noActiveProblemError) pure mbProblem

    let (names, language) = findLanguage settings lang
    generateScaffold language problem

    Problem.modifyProblemState problem $ \s -> s{ Problem.currentLanguage = NE.head names }

    let features = mapMaybe findFeature . enabledFeatureNames $ settings
    forM_ (GlobalTemplate.hook : features) (`onProblemCodeCreated` problem)
