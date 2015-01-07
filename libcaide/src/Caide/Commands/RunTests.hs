{-# LANGUAGE OverloadedStrings #-}

module Caide.Commands.RunTests (
      cmd
    , cmdEvaluate
) where


import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)
import Control.Monad.State (liftIO)

import Data.Either (isRight)
import Data.List (group, sort)
import Data.Text (Text)
import Data.Text.Read as TextRead
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Prelude hiding (FilePath)
import Filesystem (listDirectory, readTextFile, writeTextFile)
import Filesystem.Path (FilePath, (</>),  replaceExtension)
import Filesystem.Path.CurrentOS (decodeString)

import Caide.Configuration (getActiveProblem, getBuilder, readProblemConfig)
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

runTests :: [String] -> CaideIO ()
runTests _ = do
    probId <- getActiveProblem
    builderName <- getBuilder
    let builder = findBuilder builderName
    buildResult <- builder probId
    case buildResult of
        BuildFailed -> throw "Build failed"
        TestsFailed -> throw "Tests failed"
        TestsPassed -> return ()
        NoEvalTests -> evalTests []

evalTests :: [String] -> CaideIO ()
evalTests _ = do
    probId <- getActiveProblem
    root <- caideRoot
    hProblem <- readProblemConfig probId
    precision <- getProp hProblem "problem" "double_precision"
    let testsDir = root </> decodeString probId </> ".caideproblem" </> "test"
        reportFile = testsDir </> "report.txt"
        cmpOptions = ComparisonOptions { doublePrecision = precision }

    errors <- liftIO $ do
        report <- generateReport cmpOptions testsDir
        writeTextFile reportFile . serializeTestReport $ report
        T.putStrLn "Results summary\n_______________\nOutcome\tCount"
        T.putStrLn $ humanReadableSummary report
        return [r | r@(_, Error _) <- report]

    unless (null errors) $
        throw $ T.unpack $ humanReadableReport errors


generateReport :: ComparisonOptions -> FilePath -> IO (TestReport Text)
generateReport cmpOptions testDir = do
    testList <- readTests $ testDir </> "testList.txt"
    allFiles <- listDirectory testDir
    let (testNames, inputFiles) = unzip [(testName, inputFile) | (testName, _) <- testList,
                                          let inputFile = testDir </> decodeString (testName ++ ".in")
                                        ]
    results <- mapM (testResult cmpOptions allFiles) inputFiles
    return $ zip testNames results


testResult :: ComparisonOptions -> [FilePath] -> FilePath -> IO (ComparisonResult Text)
testResult cmpOptions allFiles testFile = case () of
    _ | outFile `elem` allFiles     -> if etalonFile `elem` allFiles
                                       then compareFiles cmpOptions <$> readTextFile etalonFile <*> readTextFile outFile
                                       else return EtalonUnknown
      | failedFile `elem` allFiles  -> return . Error $ "Program crashed"
      | skippedFile `elem` allFiles -> return Skipped
      | otherwise                   -> return . Error $ "unknown error"
  where [outFile, etalonFile, failedFile, skippedFile] = map (replaceExtension testFile)
          ["out", "etalon", "failed", "skipped"]


compareFiles :: ComparisonOptions -> Text -> Text -> ComparisonResult Text
compareFiles cmpOptions etalon out = case () of
    _ | not (null errors) -> Error $ T.concat ["Line ", tshow line, ": ", err]
      | length actual == length expected -> Success
      | otherwise -> Error $ T.concat ["Expected ", tshow (length expected), " line(s)"]
  where
    expected = T.lines etalon
    actual   = T.lines out
    lineComparison = zipWith (compareLines cmpOptions) expected actual
    errors = [e | e@(_, Error _) <- zip [1::Int ..] lineComparison]
    (line, Error err) = head errors


compareLines :: ComparisonOptions -> Text -> Text -> ComparisonResult Text
compareLines cmpOptions expectedLine actualLine = case () of
    _ | not (null errors) -> Error $ T.concat ["Token ", tshow numToken, ": ", err]
      | length actual == length expected -> Success
      | otherwise   ->  Error $ T.concat ["Expected ", tshow (length expected), " token(s)"]
  where
    expected = T.words expectedLine
    actual = T.words actualLine
    tokenComparison = zipWith (compareTokens cmpOptions) expected actual
    errors = [e | e@(_, Error _) <- zip [1::Int ..] tokenComparison]
    (numToken, Error err) = head errors

compareTokens :: ComparisonOptions -> Text -> Text -> ComparisonResult Text
compareTokens cmpOptions expected actual = case () of
    _ | expected == actual -> Success
      | areEqualDoubles (doublePrecision cmpOptions) expected actual -> Success
      | otherwise -> Error $ T.concat ["Expected ", expected, ", found ", actual]

areEqualDoubles :: Double -> Text -> Text -> Bool
areEqualDoubles precision expected actual = isRight expectedParsed && isRight actualParsed &&
    T.null expectedRest && T.null actualRest && abs (expectedDouble - actualDouble) <= precision
  where
    expectedParsed :: Either String (Double, Text)
    expectedParsed = TextRead.double expected
    Right (expectedDouble, expectedRest) = expectedParsed
    actualParsed = TextRead.double actual
    Right (actualDouble, actualRest) = actualParsed



tshow :: (Show a) => a -> Text
tshow = T.pack . show
