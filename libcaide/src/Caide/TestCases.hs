{-# LANGUAGE OverloadedStrings #-}
module Caide.TestCases(
      updateTests
) where

import Prelude hiding (FilePath)
import Control.Monad.Extended (MonadIO, liftIO, forM_, whenM)
import Data.Either.Util (whenLeft)
import Data.Function ((&))
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import System.Environment (getExecutablePath)


import Filesystem.Path.CurrentOS (FilePath, (</>), hasExtension)
import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem as FS
import Filesystem.Util (copyFileToDir, listDir, pathToText, )

import Caide.Commands.ConvertTestCase (convertTestCase,
    TestCasePartType(TestCaseInput, TestCaseOutput))
import Caide.Logger (logError)
import qualified Caide.Paths as Paths
import Caide.TestCases.Types (TestList, TestState(..), TestRunResult(testRunStatus),
    isSuccessful, readTestReport, writeTests)
import Caide.Types (Problem)

updateTests :: MonadIO m => FilePath -> Problem -> m TestList
updateTests problemDir problem = liftIO $ do
    let tempTestDir = problemDir </> Paths.testsDir
    FS.createTree tempTestDir

    -- Cleanup output from previous test run.
    filesToClear <- (filter (`hasExtension` "out") . fst) <$> listDir tempTestDir
    forM_ filesToClear FS.removeFile

    allFiles <- (sort . fst) <$> listDir problemDir

    -- Copy input files.
    -- TODO: don't.
    let testInputs = filter (`hasExtension` "in") allFiles
    forM_ testInputs $ \inFile -> copyFileToDir inFile tempTestDir

    let testNameFromPath = pathToText . FS.basename
        allTests = map testNameFromPath testInputs

    -- Update testList.txt file:
    -- * remove missing tests
    -- * add new tests
    -- * make sure previously failed tests (if any) come first
    let previousRunFile = problemDir </> Paths.testReportFile
    report <- readTestReport previousRunFile
    let testNameToStatus = Map.fromList [(name, testRunStatus res) | (name, res) <- report]
        testsToSkip = allFiles & filter (`hasExtension` "skip") & map testNameFromPath & Set.fromList
        testState testName = if testName `Set.member` testsToSkip then Skip else Run
        testList = zip allTests (map testState allTests)
        sortOrder (testName, _) = case Map.lookup testName testNameToStatus of
            Nothing -> (0 :: Int, testName)
            Just status | isSuccessful status == Just False -> (1, testName)
            _ -> (2, testName)
        sortedTests = sortOn sortOrder testList
    writeTests sortedTests $ problemDir </> Paths.testListFile

    -- TODO: Avoid caideExe.txt files?
    caideExe <- getExecutablePath
    FS.writeTextFile (problemDir </> Paths.testsDir </> "caideExe.txt") $ T.pack caideExe

    -- Convert sample inputs/outputs to plain format.
    forM_ allTests $ \testName -> do
        let input = problemDir </> Paths.testInput testName
        whenM (FS.isFile input) $ do
            err <- convertTestCase TestCaseInput problem
                input
                (problemDir </> Paths.convertedTestInput testName)
            whenLeft err $ logError

        let etalonOutput = problemDir </> Paths.etalonTestOutput testName
        whenM (FS.isFile etalonOutput) $ do
            err <- convertTestCase TestCaseOutput problem
                etalonOutput
                (problemDir </> Paths.convertedEtalonTestOutput testName)
            whenLeft err $ logError

    return sortedTests

