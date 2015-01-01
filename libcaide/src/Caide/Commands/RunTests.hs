{-# LANGUAGE OverloadedStrings #-}

module Caide.Commands.RunTests (
      cmd
    , cmdEvaluate
) where


import Control.Applicative ((<$>), (<*>))

import Data.List (group, sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Prelude hiding (FilePath)
import Filesystem (listDirectory, readTextFile, writeTextFile)
import Filesystem.Path (FilePath, (</>),  replaceExtension)
import Filesystem.Path.CurrentOS (decodeString)

import Caide.Configuration (getActiveProblem, getBuilder)
import Caide.Registry (findBuilder)
import Caide.Types
import Caide.TestCases.Types

cmd :: CommandHandler
cmd =  CommandHandler
    { command = "test"
    , description = "Run tests and generate test report"
    , usage = "caide test"
    , action = runTests
    }

cmdEvaluate :: CommandHandler
cmdEvaluate =  CommandHandler
    { command = "eval_tests"
    , description = "(Internal) Generate test report"
    , usage = "caide eval_tests"
    , action = evalTests
    }


humanReadableReport :: TestReport Text -> Text
humanReadableReport = T.unlines .
            map (\(testName, res) -> T.concat [T.pack testName, ": ", humanReadable res])

humanReadableSummary :: TestReport Text -> Text
humanReadableSummary = T.unlines . map toText . group . sort . map (fromComparisonResult . snd)
    where toText list = T.concat [head list, "\t", tshow (length list)]
          fromComparisonResult (Error _) = "Error"
          fromComparisonResult r = tshow r

runTests :: CaideEnvironment -> [String] -> IO (Maybe String)
runTests env _ = do
    probId <- getActiveProblem env
    builderName <- getBuilder env
    let builder = findBuilder builderName
    buildResult <- builder env probId
    case buildResult of
        BuildFailed -> return . Just $ "Build failed"
        TestsFailed -> return . Just $ "Tests failed"
        TestsPassed -> return Nothing
        TestsNotRun -> evalTests env []

evalTests :: CaideEnvironment -> [String] -> IO (Maybe String)
evalTests env _ = do
    probId <- getActiveProblem env
    let caideRoot = getRootDirectory env
        testsDir = caideRoot </> decodeString probId </> ".caideproblem" </> "test"
        reportFile = testsDir </> "report.txt"

    report <- generateReport testsDir
    writeTextFile reportFile . serializeTestReport $ report
    T.putStrLn "Results summary\n_______________\nOutcome\tCount"
    T.putStrLn $ humanReadableSummary report
    let nonSuccesses = [r | r@(_, res) <- report, res /= Success]
    if null nonSuccesses
        then return Nothing
        else return . Just . T.unpack $ humanReadableReport nonSuccesses


generateReport :: FilePath -> IO (TestReport Text)
generateReport testDir = do
    testList <- readTests $ testDir </> "testList.txt"
    allFiles <- listDirectory testDir
    let (testNames, inputFiles) = unzip [(testName, inputFile) | (testName, _) <- testList,
                                          let inputFile = testDir </> decodeString (testName ++ ".in")
                                        ]
    results <- mapM (testResult allFiles) inputFiles
    return $ zip testNames results


testResult :: [FilePath] -> FilePath -> IO (ComparisonResult Text)
testResult allFiles testFile = case () of
    _ | outFile `elem` allFiles     -> if etalonFile `elem` allFiles
                                       then compareFiles <$> readTextFile etalonFile <*> readTextFile outFile
                                       else return EtalonUnknown
      | failedFile `elem` allFiles  -> return . Error $ "Program crashed"
      | skippedFile `elem` allFiles -> return Skipped
      | otherwise                   -> return . Error $ "unknown error"
  where [outFile, etalonFile, failedFile, skippedFile] = map (replaceExtension testFile)
          ["out", "etalon", "failed", "skipped"]


compareFiles :: Text -> Text -> ComparisonResult Text
compareFiles etalon out = case () of
    _ | not (null errors) -> Error $ T.concat ["Line ", tshow line, ": ", err]
      | length actual == length expected -> Success
      | otherwise -> Error $ T.concat ["Expected ", tshow (length expected), " line(s)"]
  where
    expected = T.lines etalon
    actual   = T.lines out
    lineComparison = zipWith compareLines expected actual
    errors = [e | e@(_, Error _) <- zip [1::Int ..] lineComparison]
    (line, Error err) = head errors


compareLines :: Text -> Text -> ComparisonResult Text
compareLines expectedLine actualLine = case () of
    _ | not (null errors) -> Error $ T.concat ["Token ", tshow numToken, ": ", err]
      | length actual == length expected -> Success
      | otherwise   ->  Error $ T.concat ["Expected ", tshow (length expected), " token(s)"]
  where
    expected = T.words expectedLine
    actual = T.words actualLine
    tokenComparison = zipWith compareTokens expected actual
    errors = [e | e@(_, Error _) <- zip [1::Int ..] tokenComparison]
    (numToken, Error err) = head errors

compareTokens :: Text -> Text -> ComparisonResult Text
compareTokens expected actual =
    if expected == actual
    then Success
    else Error $ T.concat ["Expected ", expected, ", found ", actual]

tshow :: (Show a) => a -> Text
tshow = T.pack . show
