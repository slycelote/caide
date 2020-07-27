{-# LANGUAGE CPP, OverloadedStrings #-}

module Caide.Commands.RunTests(
      runTests
    , evalTests
) where


#ifndef AMP
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Monad (forM, unless)
import Control.Monad.State (liftIO)

import Data.Either (fromRight, isRight)
import Data.List (group, sort)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text.Read as TextRead
import qualified Data.Text as T
import qualified Data.Text.IO.Util as T

import Prelude hiding (FilePath)
import Filesystem (isFile, readTextFile, writeTextFile)
import Filesystem.Path (FilePath, (</>), basename, filename, hasExtension, replaceExtension)
import Filesystem.Path.CurrentOS (fromText)
import Filesystem.Util (listDir, pathToText)

import qualified Caide.Builders.None as None
import qualified Caide.Builders.Custom as Custom
import Caide.Configuration (getActiveProblem, readProblemConfig, readProblemState, readCaideConf, withDefault)
import qualified Caide.Paths as Paths
import Caide.Registry (findLanguage)
import Caide.Types
import Caide.TestCases.Types
import Caide.TestCases.TopcoderComparator
import Caide.Util (tshow)


humanReadableReport :: TestReport Text -> Text
humanReadableReport = T.unlines .
            map (\(testName, res) -> T.concat [testName, ": ", humanReadable res])

humanReadableSummary :: TestReport Text -> Text
humanReadableSummary = T.unlines . map toText . group . sort . map (fromComparisonResult . snd)
    where toText list = T.concat [head list, "\t", tshow (length list)]
          fromComparisonResult (Error _) = "Error"
          fromComparisonResult (Failed _) = "Failed"
          fromComparisonResult r = tshow r

getBuilder :: Text -> CaideIO Builder
getBuilder language = do
    h <- readCaideConf
    let builderExists langName = withDefault False $
            (getProp h langName "build_and_run_tests" :: CaideIO Text) >> return True
        languageNames = fromMaybe [] $ fst <$> findLanguage language
    buildersExist <- mapM (builderExists . T.unpack) languageNames
    let existingBuilderNames = [name | (name, True) <- zip languageNames buildersExist]
    return $ case existingBuilderNames of
        [] -> None.builder
        (name:_) -> Custom.builder name


runTests :: CaideIO ()
runTests = do
    probId <- getActiveProblem
    h <- readProblemState probId
    lang <- getProp h "problem" "language"
    builder <- getBuilder lang
    buildResult <- builder probId
    case buildResult of
        BuildFailed -> throw "Build failed"
        TestsFailed -> throw "Tests failed"
        TestsPassed -> return ()
        NoEvalTests -> evalTests

data ComparisonOptions = ComparisonOptions
    { doublePrecision :: Double
    , topcoderType :: Maybe TopcoderValue
    }

evalTests :: CaideIO ()
evalTests = do
    probId <- getActiveProblem
    root <- caideRoot
    hProblem <- readProblemConfig probId
    precision <- getProp hProblem "problem" "double_precision"
    probType  <- getProp hProblem "problem" "type"
    let problemDir = Paths.problemDir root probId
        testsDir = problemDir </> Paths.testsDir
        reportFile = testsDir </> Paths.testReportFile
        cmpOptions = ComparisonOptions
            { doublePrecision = precision
            , topcoderType = case probType of
                Topcoder descr -> Just . tcMethod $ descr
                _              -> Nothing
            }

    errors <- liftIO $ do
        report <- generateReport cmpOptions problemDir
        writeTextFile reportFile . serializeTestReport $ report
        T.putStrLn "Results summary\n_______________\nOutcome\tCount"
        T.putStrLn $ humanReadableSummary report
        return [r | r@(_, res) <- report, isSuccessful res == Just False]

    unless (null errors) $
        throw $ humanReadableReport errors


generateReport :: ComparisonOptions -> FilePath -> IO (TestReport Text)
generateReport cmpOptions problemDir = do
    let testDir = problemDir </> Paths.testsDir
    testList <- (map filename . filter (`hasExtension` "in") . fst) <$> listDir problemDir
    report   <- readTestReport $ testDir </> Paths.testReportFile
    let testNames = map (pathToText . basename) testList
    results <- forM testList $ \testFile -> do
        let testName = pathToText $ basename testFile
            inFile = problemDir </> testFile
            outFile = problemDir </> Paths.testsDir </> replaceExtension testFile "out"
            etalonFile = problemDir </> replaceExtension testFile "out"
        case lookup testName report of
            Nothing -> return $ Error "Test was not run"
            Just Ran -> do
                [etalonExists, outFileExists] <- mapM isFile [etalonFile, outFile]
                if not outFileExists
                   then return $ Error "Output file is missing"
                   else if etalonExists
                       then compareFiles cmpOptions <$> readTextFile etalonFile <*> readTextFile outFile
                       else return EtalonUnknown
            Just result -> return result

    return $ zip testNames results


compareFiles :: ComparisonOptions -> Text -> Text -> ComparisonResult Text
compareFiles cmpOptions etalon out = case () of
    _ | isTopcoder -> tcComparison
      | not (null errors) -> Failed $ T.concat ["Line ", tshow line, ": ", err]
      | length actual == length expected -> Success
      | otherwise -> Failed $ T.concat ["Expected ", tshow (length expected), " line(s)"]
  where
    Just returnValueType = topcoderType cmpOptions
    isTopcoder = isJust $ topcoderType cmpOptions
    tcComparison = case tcCompare returnValueType (doublePrecision cmpOptions) etalon out of
        Nothing    -> Success
        Just tcErr -> Failed tcErr

    expected = T.lines . T.strip $ etalon
    actual   = T.lines . T.strip $ out
    lineComparison = zipWith (compareLines cmpOptions) expected actual
    errors = [e | e@(_, Failed _) <- zip [1::Int ..] lineComparison]
    (line, Failed err) = head errors


compareLines :: ComparisonOptions -> Text -> Text -> ComparisonResult Text
compareLines cmpOptions expectedLine actualLine = case () of
    _ | not (null errors) -> Failed $ T.concat ["Token ", tshow numToken, ": ", err]
      | length actual == length expected -> Success
      | otherwise   ->  Failed $ T.concat ["Expected ", tshow (length expected), " token(s)"]
  where
    expected = T.words expectedLine
    actual = T.words actualLine
    tokenComparison = zipWith (compareTokens cmpOptions) expected actual
    errors = [e | e@(_, Failed _) <- zip [1::Int ..] tokenComparison]
    (numToken, Failed err) = head errors

compareTokens :: ComparisonOptions -> Text -> Text -> ComparisonResult Text
compareTokens cmpOptions expected actual = case () of
    _ | expected == actual -> Success
      | areEqualDoubles (doublePrecision cmpOptions) expected actual -> Success
      | otherwise -> Failed $ T.concat ["Expected ", expected, ", found ", actual]

areEqualDoubles :: Double -> Text -> Text -> Bool
areEqualDoubles precision expected actual = isRight expectedParsed && isRight actualParsed &&
    T.null expectedRest && T.null actualRest && abs (expectedDouble - actualDouble) <= precision
  where
    expectedParsed :: Either String (Double, Text)
    expectedParsed = TextRead.double expected
    Right (expectedDouble, expectedRest) = expectedParsed
    actualParsed = TextRead.double actual
    Right (actualDouble, actualRest) = actualParsed

