{-# LANGUAGE OverloadedStrings #-}
module Caide.Paths(
      problemDir
    , problemConfFile
    , problemStateFile
    , testsDir
    , testInput
    , etalonTestOutput
    , convertedTestInput
    , convertedEtalonTestOutput
    , userTestOutput
    , testReportFile
    , testListFile
    , caideConfFile
    , caideStateFile
) where

import Prelude hiding (FilePath)

import Data.Text (Text)

import qualified Filesystem.Path.CurrentOS as FS
import Filesystem.Path.CurrentOS (FilePath, (</>))

import Caide.Types (ProblemID)


-- | Relative to root directory.
caideConfFile :: FilePath
caideConfFile = "caide.ini"

-- | Relative to root directory.
caideStateFile :: FilePath
caideStateFile = ".caide" </> "config"

problemDir :: FilePath -> ProblemID -> FilePath
problemDir root probId = root </> FS.fromText probId

-- | Relative to 'problemDir'
problemConfFile :: FilePath
problemConfFile = "problem.ini"

-- | Relative to 'problemDir'
problemStateFile :: FilePath
problemStateFile = ".caideproblem" </> "config"

-- | Relative to 'problemDir'
testsDir :: FilePath
testsDir =  ".caideproblem" </> "test"

-- | Relative to 'problemDir'
testReportFile :: FilePath
testReportFile = testsDir </> "report.txt"

-- | Relative to 'problemDir'
testListFile :: FilePath
testListFile = testsDir </> "testList.txt"

-- | Relative to 'problemDir'
testInput :: Text -> FilePath
testInput testName = FS.addExtension (FS.fromText testName) "in"

-- | Relative to 'problemDir'
etalonTestOutput :: Text -> FilePath
etalonTestOutput testName = FS.addExtension (FS.fromText testName) "out"

-- | Relative to 'problemDir'
convertedTestInput :: Text -> FilePath
convertedTestInput testName = testsDir </> FS.addExtension (FS.fromText testName) "plain.in"

-- | Relative to 'problemDir'
convertedEtalonTestOutput :: Text -> FilePath
convertedEtalonTestOutput testName = testsDir </> FS.addExtension (FS.fromText testName) "plain.etalon"

-- | Relative to 'problemDir'
userTestOutput :: Text -> FilePath
userTestOutput testName = testsDir </> FS.addExtension (FS.fromText testName) "out"

