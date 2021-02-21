{-# LANGUAGE OverloadedStrings #-}
module Caide.TestCases(
      updateTests
    , updateTestList
) where

import Prelude hiding (FilePath)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Environment (getExecutablePath)


import Filesystem.Path.CurrentOS (FilePath, (</>), basename, hasExtension)
import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem as FS
import Filesystem.Util (copyFileToDir, pathToText)

import qualified Caide.Paths as Paths
import Caide.TestCases.Types (TestList, TestState(..), TestRunResult(testRunStatus),
    isSuccessful, readTestReport, writeTests)


updateTests :: MonadIO m => FilePath -> m ()
updateTests problemDir = liftIO $ do
    copyTestInputs problemDir
    _ <- updateTestList problemDir
    -- TODO: Avoid caideExe.txt files?
    caideExe <- getExecutablePath
    FS.writeTextFile (problemDir </> Paths.testsDir </> "caideExe.txt") $ T.pack caideExe

copyTestInputs :: FilePath -> IO ()
copyTestInputs problemDir = do
    let tempTestDir = problemDir </> Paths.testsDir
    FS.createTree tempTestDir

    -- Cleanup output from previous test run
    let filesToKeep = ["testList.txt", "report.txt", "caideExe.txt"]
    filesToClear <- filter ((`notElem` filesToKeep) . FS.encodeString . FS.filename) <$> FS.listDirectory tempTestDir
    forM_ filesToClear FS.removeFile

    fileList <- FS.listDirectory problemDir
    -- Copy input files.
    -- TODO: don't.
    let testInputs = filter (`hasExtension` "in") fileList
    forM_ testInputs $ \inFile -> copyFileToDir inFile tempTestDir


-- | Updates testList.txt file:
--    * removes missing tests
--    * adds new tests
--    * makes sure previously failed tests (if any) come first
updateTestList :: FilePath -> IO TestList
updateTestList problemDir = do
    let testDir = problemDir </> Paths.testsDir
        previousRunFile = testDir </> Paths.testReportFile
    report <- readTestReport previousRunFile
    let testNameToStatus = Map.fromList [(name, testRunStatus res) | (name, res) <- report]
    allFiles <- sort <$> FS.listDirectory problemDir
    let allTests    = map (pathToText . basename) . filter (`hasExtension` "in") $ allFiles
        testsToSkip = map (pathToText . basename) . filter (`hasExtension` "skip") $ allFiles
        testState testName = if testName `elem` testsToSkip then Skip else Run
        testList = zip allTests (map testState allTests)
        succeededAndName (testName, _) = case isSuccessful =<< Map.lookup testName testNameToStatus of
            Just False -> (False, testName)
            _          -> (True,  testName)
        sortedTests = sortOn succeededAndName testList
    writeTests sortedTests $ testDir </> Paths.testListFile
    return sortedTests

