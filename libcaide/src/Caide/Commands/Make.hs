{-# LANGUAGE OverloadedStrings #-}
module Caide.Commands.Make(
      updateTests
    , make
) where

import Control.Monad.Extended (liftIO, void)

import Prelude hiding (FilePath)
import Filesystem (isDirectory, )
import Filesystem.Path.CurrentOS (FilePath, )

import Caide.GlobalState (readGlobalState, activeProblem, noActiveProblemError)
import Caide.Monad (CaideIO, caideRoot, throw)
import qualified Caide.Problem as Problem
import qualified Caide.Paths as Paths
import Caide.Registry (findLanguage)
import Caide.Types
import Caide.Types.ProgrammingLanguage (inlineCode)
import qualified Caide.TestCases as TestCases


withProblem :: Maybe ProblemID -> (Problem -> FilePath -> CaideIO a) -> CaideIO a
withProblem mbProbId processProblem = do
    mbProbId' <- maybe (activeProblem <$> readGlobalState) (pure.Just) mbProbId
    probId <- maybe (throw noActiveProblemError) pure mbProbId'
    root <- caideRoot
    let probDir = Paths.problemDir root probId
    problemExists <- liftIO $ isDirectory probDir
    if problemExists
    then do
        problem <- Problem.readProblemInfo probId
        processProblem problem probDir
    else throw $ "Problem " <> probId <> " doesn't exist"

make :: Maybe ProblemID -> CaideIO ()
make p = withProblem p $ \problem probDir -> do
    -- TODO: don't update tests here
    _ <- TestCases.updateTests probDir problem
    problemState <- Problem.readProblemState (problemId problem)
    let lang = Problem.currentLanguage problemState
    case findLanguage lang of
        Nothing            -> throw $ "Unsupported programming language " <> lang
        Just (_, language) -> inlineCode language (problemId problem)

updateTests :: Maybe ProblemID -> CaideIO ()
updateTests mbProbId = withProblem mbProbId $ \problem problemDir ->
    void $ TestCases.updateTests problemDir problem

