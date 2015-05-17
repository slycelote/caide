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


data ComparisonResult a = Success        -- ^ Test passed
                        | Ran            -- ^ Test ran but hasn't been evaluated
                        | EtalonUnknown  -- ^ Test ran but etalon is unknown
                        | Skipped        -- ^ Test was skipped
                        | Failed a       -- ^ Test failed
                        | Error a        -- ^ Error (e.g. exception in checker)
                        deriving (Show, Eq)

humanReadable :: ComparisonResult Text -> Text
humanReadable Success = "OK"
humanReadable Ran = "ran"
humanReadable Skipped = "skipped"
humanReadable (Failed message) = T.append "failed: " message
humanReadable EtalonUnknown = "unknown"
humanReadable (Error err) = err

machineReadable :: ComparisonResult Text -> Text
machineReadable (Failed message) = T.append "failed " message
machineReadable (Error err) = T.append "error " err
machineReadable res = humanReadable res


type TestReport a = [(Text, ComparisonResult a)]

serializeTestReport :: TestReport Text -> Text
serializeTestReport = T.unlines .
            map (\(testName, res) -> T.concat [testName, " ", machineReadable res])

readComparisonResult :: [Text] -> ComparisonResult Text
readComparisonResult ["OK"] = Success
readComparisonResult ["ran"] = Ran
readComparisonResult ["skipped"] = Skipped
readComparisonResult ["unknown"] = EtalonUnknown
readComparisonResult ("failed":err) = Failed $ T.concat err
readComparisonResult ("error":err) = Error $ T.concat err
readComparisonResult _ = Error "Corrupted report file"

deserializeTestReport :: Text -> TestReport Text
deserializeTestReport text = map (parseTest . T.words) reportLines
  where
    reportLines = filter (not . T.null) . map T.strip $ T.lines text

    parseTest :: [Text] -> (Text, ComparisonResult Text)
    parseTest (testName:testStatus:err) = (testName, readComparisonResult (testStatus:err))
    parseTest [testName] = (testName, Error "Corrupted report file")
    parseTest [] = error "Impossible happened in deserializeTestReport"

readTestReport :: FilePath -> IO (TestReport Text)
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

