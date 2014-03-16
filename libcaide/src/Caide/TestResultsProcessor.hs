module Caide.TestResultsProcessor(
      generateReport
) where

import Control.Applicative ((<$>))
import qualified Data.Text as T
import Filesystem (listDirectory, readTextFile)
import qualified Filesystem.Path as F
import Filesystem.Path.CurrentOS (basename, encodeString, hasExtension, replaceExtension,)

generateReport :: F.FilePath -> IO T.Text
generateReport testDir = do
    allFiles <- listDirectory testDir
    let inputFiles = [f | f <- allFiles, hasExtension f (T.pack "in")]
        testNames = map (T.pack . encodeString . basename) inputFiles
    results <- mapM (testResult allFiles) inputFiles
    let reports = zipWith (\testName result -> T.concat [testName, T.pack ": ", result]) testNames results
    return $ T.unlines reports

testResult :: [F.FilePath] -> F.FilePath -> IO T.Text
testResult allFiles testFile = do
    let [outFile, etalonFile, failedFile, skippedFile] = map (replaceExtension testFile . T.pack)
          ["out", "etalon", "failed", "skipped"]
    case () of
      _ | outFile `elem` allFiles     -> compareFiles outFile etalonFile
        | failedFile `elem` allFiles  -> return $ T.pack "Program crashed"
        | skippedFile `elem` allFiles -> return $ T.pack "skipped"
        | otherwise                   -> return $ T.pack "unknown error"

compareFiles :: F.FilePath -> F.FilePath -> IO T.Text
compareFiles outFile etalonFile = do
    output <- T.lines <$> readTextFile outFile
    etalon <- T.lines <$> readTextFile etalonFile
    -- TODO: ignore whitespace, handle floating point numbers correctly etc.
    return . T.pack $ if output == etalon then "OK" else "Fail"
