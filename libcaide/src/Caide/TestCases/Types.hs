{-# LANGUAGE OverloadedStrings #-}

module Caide.TestCases.Types (
      ComparisonResult (..)
    , humanReadable
    , machineReadable

    , TestReport
    , serializeTestReport
    , deserializeTestReport
    , readTestReport

    , TestState (..)
    , TestList
    , serializeTestList
    , deserializeTestList
    , readTests
    , writeTests
) where


import Control.Applicative ((<$>))
import Data.Text (Text)
import qualified Data.Text as T

import Prelude hiding (FilePath)
import Filesystem.Path (FilePath)
import Filesystem (isFile, readTextFile, writeTextFile)
import Caide.Util (tshow)


data ComparisonResult a = Success | Skipped | EtalonUnknown | Error a
                            deriving (Show, Eq)

humanReadable :: ComparisonResult Text -> Text
humanReadable Success = "OK"
humanReadable Skipped = "skipped"
humanReadable EtalonUnknown = "unknown"
humanReadable (Error err) = err

machineReadable :: ComparisonResult Text -> Text
machineReadable (Error _) = "failed"
machineReadable res = humanReadable res


type TestReport a = [(Text, ComparisonResult a)]

serializeTestReport :: TestReport Text -> Text
serializeTestReport = T.unlines .
            map (\(testName, res) -> T.concat [testName, " ", machineReadable res])

deserializeTestReport :: Text -> TestReport ()
deserializeTestReport text = map (parseTest . T.words) reportLines
  where
    reportLines = T.lines text

    parseTest :: [Text] -> (Text, ComparisonResult ())
    parseTest [testName, testStatus] = (testName, if testStatus == "failed" then Error () else Success)
    parseTest _ = error "Corrupted test report"

readTestReport :: FilePath -> IO (TestReport ())
readTestReport reportFile = do
    reportExists <- isFile reportFile
    if reportExists
        then deserializeTestReport <$> readTextFile reportFile
        else return []

data TestState = Run | Skip deriving (Read, Show)

type TestList = [(Text, TestState)]

serializeTestList :: TestList -> Text
serializeTestList = T.unlines . map (\(name, state) -> T.concat [name, " ", tshow state])

deserializeTestList :: Text -> TestList
deserializeTestList text = map toTest testLines
  where
    testLines = map T.words . T.lines $ text

    toTest [name, state] = (name, read $ T.unpack state)
    toTest _ = error "Corrupted testList file"

readTests :: FilePath -> IO TestList
readTests testListFile = do
    testListExists <- isFile testListFile
    if testListExists
    then deserializeTestList <$> readTextFile testListFile
    else return []

writeTests :: TestList -> FilePath -> IO ()
writeTests tests testListFile = writeTextFile testListFile $ serializeTestList tests

