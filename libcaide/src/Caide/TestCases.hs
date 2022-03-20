{-# LANGUAGE OverloadedStrings #-}
module Caide.TestCases(
      updateTests
) where

import Prelude hiding (FilePath)
import Control.Monad.Extended (MonadIO, liftIO, forM_)
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Environment (getExecutablePath)


import Filesystem.Path.CurrentOS (FilePath, (</>), basename, hasExtension)
import qualified Filesystem as FS
import Filesystem.Util (copyFileToDir, listDir, pathToText, )

import qualified Caide.Paths as Paths
import Caide.TestCases.Types (TestList, TestState(..), TestRunResult(testRunStatus),
    isSuccessful, readTestReport, writeTests)


updateTests :: MonadIO m => FilePath -> m TestList
updateTests problemDir = liftIO $ do
    let tempTestDir = problemDir </> Paths.testsDir
    FS.createTree tempTestDir

    -- Cleanup output from previous test run
    filesToClear <- (filter (`hasExtension` "out") . fst) <$> listDir tempTestDir
    forM_ filesToClear FS.removeFile

    -- Copy input files.
    -- TODO: don't.
    fileList <- fst <$> listDir problemDir
    let testInputs = filter (`hasExtension` "in") fileList
    forM_ testInputs $ \inFile -> copyFileToDir inFile tempTestDir

    -- Update testList.txt file:
    -- * remove missing tests
    -- * add new tests
    -- * make sure previously failed tests (if any) come first
    let previousRunFile = problemDir </> Paths.testReportFile
    report <- readTestReport previousRunFile
    let testNameToStatus = Map.fromList [(name, testRunStatus res) | (name, res) <- report]
    allFiles <- (sort . fst) <$> listDir problemDir
    let allTests    = map (pathToText . basename) . filter (`hasExtension` "in") $ allFiles
        testsToSkip = map (pathToText . basename) . filter (`hasExtension` "skip") $ allFiles
        testState testName = if testName `elem` testsToSkip then Skip else Run
        testList = zip allTests (map testState allTests)
        succeededAndName (testName, _) = case isSuccessful =<< Map.lookup testName testNameToStatus of
            Just False -> (False, testName)
            _          -> (True,  testName)
        sortedTests = sortOn succeededAndName testList
    writeTests sortedTests $ problemDir </> Paths.testListFile

    -- TODO: Avoid caideExe.txt files?
    caideExe <- getExecutablePath
    FS.writeTextFile (problemDir </> Paths.testsDir </> "caideExe.txt") $ T.pack caideExe

    return sortedTests

