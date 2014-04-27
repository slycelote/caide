module Caide.Commands.RunTests (
      cmd
) where


import Control.Applicative ((<$>))

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Prelude hiding (FilePath)
import Filesystem (listDirectory, readTextFile, writeTextFile)
import Filesystem.Path (FilePath, (</>),  replaceExtension, hasExtension, basename)
import Filesystem.Path.CurrentOS (decodeString, encodeString)

import Caide.Configuration (getActiveProblem, getBuilder, readRootConf)
import Caide.Registry (findBuilder)
import Caide.Types

cmd :: CommandHandler
cmd =  CommandHandler
    { command = "test"
    , description = "Run tests and generate test report"
    , usage = "caide test"
    , action = runTests
    }

runTests :: FilePath -> [String] -> IO ()
runTests caideRoot _ = do
    conf <- readRootConf caideRoot
    let builderName = getBuilder conf
        builder = findBuilder builderName
        probId = getActiveProblem conf
        testsDir = caideRoot </> decodeString probId </> decodeString ".caideproblem" </> decodeString "test"
        reportFile = testsDir </> decodeString "report.txt"
    buildOk <- builder caideRoot probId
    if buildOk
    then do
        report <- generateReport testsDir
        writeTextFile reportFile report
        T.putStrLn report
    else putStrLn "Test runner failed"

generateReport :: FilePath -> IO T.Text
generateReport testDir = do
    allFiles <- listDirectory testDir
    let inputFiles = filter (`hasExtension` T.pack "in") allFiles
        testNames = map (T.pack . encodeString . basename) inputFiles
    results <- mapM (testResult allFiles) inputFiles
    let reports = zipWith (\testName result -> T.concat [testName, T.pack ": ", result]) testNames results
    return $ T.unlines reports

testResult :: [FilePath] -> FilePath -> IO T.Text
testResult allFiles testFile = do
    let [outFile, etalonFile, failedFile, skippedFile] = map (replaceExtension testFile . T.pack)
          ["out", "etalon", "failed", "skipped"]
    case () of
      _ | outFile `elem` allFiles     -> compareFiles outFile etalonFile
        | failedFile `elem` allFiles  -> return $ T.pack "Program crashed"
        | skippedFile `elem` allFiles -> return $ T.pack "skipped"
        | otherwise                   -> return $ T.pack "unknown error"

compareFiles :: FilePath -> FilePath -> IO T.Text
compareFiles outFile etalonFile = do
    output <- T.lines <$> readTextFile outFile
    etalon <- T.lines <$> readTextFile etalonFile
    -- TODO: ignore whitespace, handle floating point numbers correctly etc.
    return . T.pack $ if output == etalon then "OK" else "Fail"
