{-# LANGUAGE OverloadedStrings #-}
module Caide.Paths(
      problemDir
    , testsDir
    , testReportFile
    , testListFile
    , caideConfFile
    , caideStateFile
) where

import Prelude hiding (FilePath)

import qualified Filesystem.Path.CurrentOS as FS
import Filesystem.Path.CurrentOS (FilePath, (</>))
import Caide.Types (ProblemID)

caideConfFile :: FilePath -> FilePath
caideConfFile root = root </> "caide.ini"

caideStateFile :: FilePath -> FilePath
caideStateFile root = root </> ".caide" </> "config"

problemDir :: FilePath -> ProblemID -> FilePath
problemDir root probId = root </> FS.fromText probId

-- | Relative to 'problemDir'
testsDir :: FilePath
testsDir =  ".caideproblem" </> "test"

-- | Relative to 'testsDir'
testReportFile :: FilePath
testReportFile = "report.txt"

-- | Relative to 'testsDir'
testListFile :: FilePath
testListFile = "testList.txt"

