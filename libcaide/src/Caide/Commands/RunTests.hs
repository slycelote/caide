{-# LANGUAGE OverloadedStrings #-}

module Caide.Commands.RunTests (
      cmd
) where


import Control.Applicative ((<$>), (<*>))

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

runTests :: CaideEnvironment -> [String] -> IO ()
runTests env _ = do
    probId <- getActiveProblem env
    builderName <- getBuilder env
    let caideRoot = getRootDirectory env
        builder = findBuilder builderName
        testsDir = caideRoot </> decodeString probId </> decodeString ".caideproblem" </> decodeString "test"
        reportFile = testsDir </> decodeString "report.txt"
    buildOk <- builder env probId
    if buildOk
    then do
        report <- generateReport testsDir
        writeTextFile reportFile . serializeTestReport $ report
        T.putStrLn . T.unlines $
            map (\(testName, res) -> T.concat [T.pack testName, T.pack ": ", humanReadable res]) report
    else putStrLn "Test runner failed"


generateReport :: FilePath -> IO (TestReport Text)
generateReport testDir = do
    testList <- readTests $ testDir </> decodeString "testList.txt"
    allFiles <- listDirectory testDir
    let (testNames, inputFiles) = unzip [(testName, inputFile) | (testName, _) <- testList,
                                          let inputFile = testDir </> decodeString (testName ++ ".in")
                                        ]
    results <- mapM (testResult allFiles) inputFiles
    return $ zip testNames results


testResult :: [FilePath] -> FilePath -> IO (ComparisonResult Text)
testResult allFiles testFile = do
    let [outFile, etalonFile, failedFile, skippedFile] = map (replaceExtension testFile . T.pack)
          ["out", "etalon", "failed", "skipped"]
    case () of
      _ | outFile `elem` allFiles     -> if etalonFile `elem` allFiles
                                            then compareFiles <$> readTextFile etalonFile <*> readTextFile outFile
                                            else return EtalonUnknown
        | failedFile `elem` allFiles  -> return . Error . T.pack $ "Program crashed"
        | skippedFile `elem` allFiles -> return Skipped
        | otherwise                   -> return . Error . T.pack $ "unknown error"

compareFiles :: Text -> Text -> ComparisonResult Text
compareFiles etalon out = let
    expected = T.lines etalon
    actual   = T.lines out
    lineComparison = zipWith compareLines expected actual
    errors = [e | e@(_, Error _) <- zip [1::Int ..] lineComparison]
    (line, Error err) = head errors
    in case () of
        _ | not (null errors) -> Error $ T.append (T.pack ("Line " ++ show line ++ ": ")) err
          | length actual == length expected -> Success
          | otherwise -> Error $ T.concat ["Expected ", tshow (length expected), " lines"]


compareLines :: Text -> Text -> ComparisonResult Text
compareLines expectedLine actualLine = let
    expected = T.words expectedLine
    actual = T.words actualLine
    tokenComparison = zipWith compareTokens expected actual
    errors = [e | e@(_, Error _) <- zip [1::Int ..] tokenComparison]
    (numToken, Error err) = head errors
    in case () of
      _ | not (null errors) -> Error $ T.concat [T.pack "Token ", T.pack (show numToken), T.pack ": ", err]
        | length actual == length expected -> Success
        | otherwise   ->  Error . T.pack $ "Expected " ++ show (length expected) ++ " tokens"

compareTokens :: Text -> Text -> ComparisonResult Text
compareTokens expected actual =
    if expected == actual
    then Success
    else Error $ T.concat [T.pack "Expected ", expected, T.pack ", found ", actual]

tshow :: (Show a) => a -> Text
tshow = T.pack . show

