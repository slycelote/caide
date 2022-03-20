{-# LANGUAGE OverloadedStrings #-}
module Caide.Commands.Make(
      updateTests
    , make
) where

import Control.Monad.Extended (liftIO, void)

import Prelude hiding (FilePath)
import Filesystem (isDirectory, )
import Filesystem.Path.CurrentOS (FilePath, )

import Caide.Configuration (getActiveProblem)
import qualified Caide.Problem as Problem
import qualified Caide.Paths as Paths
import Caide.Registry (findLanguage)
import Caide.Types
import qualified Caide.TestCases as TestCases


withProblem :: Maybe ProblemID -> (ProblemID -> FilePath -> CaideIO a) -> CaideIO a
withProblem mbProbId processProblem = do
    probId <- case mbProbId of
        Just probId' -> return probId'
        Nothing -> getActiveProblem
    root <- caideRoot
    let probDir = Paths.problemDir root probId
    problemExists <- liftIO $ isDirectory probDir
    if problemExists
    then processProblem probId probDir
    else throw $ "Problem " <> probId <> " doesn't exist"

make :: Maybe ProblemID -> CaideIO ()
make p = withProblem p $ \probId probDir -> do
    _ <- TestCases.updateTests probDir
    problem <- Problem.readProblemState probId
    let lang = Problem.currentLanguage problem
    case findLanguage lang of
        Nothing            -> throw $ "Unsupported programming language " <> lang
        Just (_, language) -> inlineCode language probId

updateTests :: Maybe ProblemID -> CaideIO ()
updateTests mbProbId = withProblem mbProbId $ \_probId problemDir -> void $ TestCases.updateTests problemDir

