{-# LANGUAGE OverloadedStrings #-}
module Caide.Commands.Make(
      updateTests
    , make
) where

import Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T

import Prelude hiding (FilePath)
import Filesystem (isDirectory, )
import Filesystem.Path.CurrentOS (FilePath, fromText, (</>))

import Caide.Configuration (getActiveProblem, readProblemState)
import Caide.Registry (findLanguage)
import Caide.Types
import qualified Caide.TestCases as TestCases



withProblem :: Maybe ProblemID -> (ProblemID -> FilePath -> CaideM IO a) -> CaideM IO a
withProblem mbProbId processProblem = do
    probId <- case mbProbId of
        Just probId' -> return probId'
        Nothing -> getActiveProblem
    root <- caideRoot
    let problemDir = root </> fromText probId
    problemExists <- liftIO $ isDirectory problemDir
    if problemExists
    then processProblem probId problemDir
    else throw . T.concat $ ["Problem ", probId, " doesn't exist"]

make :: Maybe ProblemID -> CaideIO ()
make p = withProblem p $ \probId _ -> do
    updateTests $ Just probId
    prepareSubmission probId

updateTests :: Maybe ProblemID -> CaideIO ()
updateTests mbProbId = withProblem mbProbId $ \_ problemDir -> TestCases.updateTests problemDir

prepareSubmission :: ProblemID -> CaideIO ()
prepareSubmission probId = do
    hProblem <- readProblemState probId
    lang <- getProp hProblem "problem" "language"
    case findLanguage lang of
        Nothing            -> throw . T.concat $ ["Unsupported programming language ", lang]
        Just (_, language) -> inlineCode language probId

