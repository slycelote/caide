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



data ComparisonResult a = Success | Skipped | EtalonUnknown | Error a deriving Show

humanReadable :: ComparisonResult Text -> Text
humanReadable Success = T.pack "OK"
humanReadable Skipped = T.pack "skipped"
humanReadable EtalonUnknown = T.pack "unknown"
humanReadable (Error err) = err

machineReadable :: ComparisonResult Text -> Text
machineReadable (Error _) = T.pack "failed"
machineReadable res = humanReadable res


type TestReport a = [(String, ComparisonResult a)]

serializeTestReport :: TestReport Text -> Text
serializeTestReport = T.unlines .
            map (\(testName, res) -> T.concat [T.pack testName, T.pack " ", machineReadable res])

deserializeTestReport :: Text -> TestReport ()
deserializeTestReport text = let
    reportLines = T.lines text

    parseTest :: [Text] -> (String, ComparisonResult ())
    parseTest [testName, testStatus] = (T.unpack testName, if testStatus == T.pack "failed" then Error () else Success)
    parseTest _ = error "Corrupted test report"

    in map (parseTest . T.words) reportLines

readTestReport :: FilePath -> IO (TestReport ())
readTestReport reportFile = do
    reportExists <- isFile reportFile
    if reportExists
        then deserializeTestReport <$> readTextFile reportFile
        else return []

data TestState = Run | Skip deriving (Read, Show)

type TestList = [(String, TestState)]

serializeTestList :: TestList -> Text
serializeTestList = T.pack . unlines . map (\(name, state) -> name ++ " " ++ show state)

deserializeTestList :: Text -> TestList
deserializeTestList text = let
    testLines = map (words . T.unpack) . T.lines $ text

    toTest [name, state] = (name, read state)
    toTest _ = error "Corrupted testList file"

    in map toTest testLines

readTests :: FilePath -> IO TestList
readTests testListFile = do
    testListExists <- isFile testListFile
    if testListExists
        then deserializeTestList <$> readTextFile testListFile
        else return []

writeTests :: TestList -> FilePath -> IO ()
writeTests tests testListFile = writeTextFile testListFile $ serializeTestList tests
