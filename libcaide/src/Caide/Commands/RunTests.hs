{-# LANGUAGE NamedFieldPuns, OverloadedStrings, ScopedTypeVariables #-}
module Caide.Commands.RunTests(
      runTests
    , evalTests
) where

import Control.Monad.Extended (liftIO, forM, unless)
import Data.Either (isRight)
import Data.Int (Int64)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text.Read as TextRead
import qualified Data.Text as T
import qualified Data.Text.IO.Util as T
import Data.Word (Word64)

import Prelude hiding (FilePath)
import Filesystem (isFile, readTextFile, writeTextFile)
import Filesystem.Path.CurrentOS (FilePath, (</>), basename, filename, hasExtension)
import Filesystem.Util (listDir, pathToText)

import qualified Data.Aeson as Aeson
import qualified Data.Scientific as Sci
import qualified Data.Vector as Vec

import qualified Caide.Builders.None as None
import qualified Caide.Builders.Custom as Custom
import Caide.CustomBuilder (createBuilderFromDirectory)
import Caide.Configuration (getActiveProblem, readCaideConf, withDefault)
import Caide.Logger (logError)
import qualified Caide.Paths as Paths
import Caide.Problem (currentLanguage, readProblemInfo, readProblemState)
import Caide.Registry (findLanguage)
import Caide.Settings (verboseTestReport)
import Caide.Types
import Caide.TestCases.Types (ComparisonResult(..), isError, isFailure, isSuccessful,
    TestRunResult(..), makeTestRunResult,
    TestReport, humanReadableReport, humanReadableSummary, readTestReport, serializeTestReport)
import qualified Caide.TestCases.TopcoderDeserializer as TC
import Caide.Util (tshow)


createBuilderFromProblemDirectory :: ProblemID -> CaideIO (Either Text Builder)
createBuilderFromProblemDirectory probId = do
    root <- caideRoot
    createBuilderFromDirectory (Paths.problemDir root probId) []

getBuilder :: Text -> ProblemID -> CaideIO Builder
getBuilder language probId = do
    h <- readCaideConf
    let builderExists langName = withDefault False $
            (getProp h langName "build_and_run_tests" :: CaideIO Text) >> return True
        languageNames = maybe [] fst $ findLanguage language
    buildersExist <- mapM (builderExists . T.unpack) languageNames
    let existingBuilderNames = [name | (name, True) <- zip languageNames buildersExist]
    case existingBuilderNames of
        (name:_) -> return $ Custom.builder name
        [] -> do
            errOrBuilder <- createBuilderFromProblemDirectory probId
            case errOrBuilder of
                Left e -> logError e >> return None.builder
                Right b -> return b

-- TODO: pass optional problem ID
runTests :: CaideIO ()
runTests = do
    probId <- getActiveProblem
    probState <- readProblemState probId
    builder <- getBuilder (currentLanguage probState) probId
    buildResult <- builder probId
    case buildResult of
        BuildFailed -> throw "Build failed"
        TestsFailed -> throw "Tests failed"
        TestsPassed -> return ()
        NoEvalTests -> evalTests

data TestFormat = PlainText | Json !(TC.Parser Aeson.Value)

data ComparisonOptions = ComparisonOptions
    { doublePrecision :: !Double
    , testFormat :: !TestFormat
    }

evalTests :: CaideIO ()
evalTests = do
    probId <- getActiveProblem
    root <- caideRoot
    problem <- readProblemInfo probId
    let problemDir = Paths.problemDir root probId
        reportFile = problemDir </> Paths.testReportFile
        cmpOptions = ComparisonOptions
            { doublePrecision = problemFloatTolerance problem
            , testFormat = case problemType problem of
                Topcoder descr -> Json $ TC.jsonParser (tcMethod . tcSingleMethod $ descr)
                Stream _ _     -> PlainText
                LeetCodeMethod _ -> Json Aeson.json
                LeetCodeClass _ _ _ -> Json Aeson.json
            }

    beVerbose <- verboseTestReport <$> caideSettings
    errorCount <- liftIO $ do
        report <- generateReport cmpOptions problemDir
        writeTextFile reportFile . serializeTestReport $ report
        T.putStrLn "Results summary\n_______________\nOutcome\tCount"
        T.putStrLn $ humanReadableSummary report
        let errors = [r | r@(_, res) <- report, isSuccessful (testRunStatus res) == Just False]
        T.putStrLn . humanReadableReport $ if beVerbose then report else errors
        return $ length errors

    unless (errorCount == 0) $
        throw $ tshow errorCount <> " tests failed!"


generateReport :: ComparisonOptions -> FilePath -> IO TestReport
generateReport cmpOptions problemDir = do
    testList <- (map filename . filter (`hasExtension` "in") . fst) <$> listDir problemDir
    report   <- readTestReport $ problemDir </> Paths.testReportFile
    let testNames = map (pathToText . basename) testList
    results <- forM testList $ \testFile -> do
        let testName = pathToText $ basename testFile
            outFile = problemDir </> Paths.userTestOutput testName
            etalonFile = problemDir </> Paths.etalonTestOutput testName
        case lookup testName report of
            Nothing -> return $ makeTestRunResult $ Error "Test was not run"
            Just res@TestRunResult{testRunStatus = Ran} -> do
                [etalonExists, outFileExists] <- mapM isFile [etalonFile, outFile]
                comparisonResult <- if not outFileExists
                   then return $ Error "Output file is missing"
                   else if etalonExists
                       then compareFiles cmpOptions <$> readTextFile etalonFile <*> readTextFile outFile
                       else return EtalonUnknown
                return $ res{testRunStatus = comparisonResult}
            Just result -> return result

    return $ sortBy (comparing fst) $ zip testNames results


compareFiles :: ComparisonOptions -> Text -> Text -> ComparisonResult
compareFiles ComparisonOptions{doublePrecision, testFormat=Json parser} etalon actual =
    case (TC.runParser parser etalon, TC.runParser parser actual) of
        (Right e, Right a) -> compareJsonValues doublePrecision e a
        (Right _, Left err) -> Failed $ "Invalid JSON output: " <> err
        (Left err, _) -> Error $ "Invalid JSON in etalon: " <> err

compareFiles ComparisonOptions{doublePrecision, testFormat=PlainText} etalon actual
  | not (null errors) = Failed $ "Line " <> tshow line <> ": " <> err
  | length actualLines == length etalonLines = Success
  | otherwise = Failed $ "Expected " <> tshow (length etalonLines) <> " line(s)"
  where
    etalonLines = T.lines . T.strip $ etalon
    actualLines = T.lines . T.strip $ actual
    lineComparison = zipWith (compareLines doublePrecision) etalonLines actualLines
    errors = [e | e@(_, Failed _) <- zip [1::Int ..] lineComparison]
    (line, Failed err) = head errors

mapError :: (Text -> Text) -> ComparisonResult -> ComparisonResult
mapError _ Success = Success
mapError _ Ran = Ran
mapError _ EtalonUnknown = EtalonUnknown
mapError _ Skipped = Skipped
mapError f (Failed e) = Failed (f e)
mapError f (Error e) = Error (f e)

compareJsonValues :: Double -> Aeson.Value -> Aeson.Value -> ComparisonResult
compareJsonValues eps etalon actual = case (etalon, actual) of
  (Aeson.Object _, _) -> Error "JSON object unexpected in etalon"
  (_, Aeson.Object _) -> Failed "JSON object unexpected"

  (Aeson.Array etalonVec, Aeson.Array actualVec) ->
    let elemCmp i e a = (i, compareJsonValues eps e a)
        comparisons = Vec.izipWith elemCmp etalonVec actualVec
        mbError = Vec.find (isError . snd) comparisons
        mbFailure = Vec.find (isFailure . snd) comparisons
        etalonLen = Vec.length etalonVec
        actualLen = Vec.length actualVec
    in case (mbError, mbFailure) of
        (Just (i,e), _) -> mapError (\s -> "[" <> tshow i <> "] " <> s) e
        (_, Just (i,f)) -> mapError (\s -> "[" <> tshow i <> "] " <> s) f
        (_, _) | etalonLen == actualLen -> Success
        _ -> Failed $ "Vector sizes differ (expected " <>
                      tshow etalonLen <> ", actual " <> tshow actualLen <> ")"
  (Aeson.Array _, _) -> Failed "Expected an array"

  (Aeson.String e, Aeson.String a) -> if e == a then Success else Failed "Strings differ"
  (Aeson.String _, _) -> Failed "Expected a string"

  (Aeson.Null, Aeson.Null) -> Success
  (Aeson.Null, _) -> Failed "Expected null"

  (Aeson.Bool e, Aeson.Bool a) -> if e == a then Success else Failed "Booleans differ"
  (Aeson.Bool _, _) -> Failed "Expected a boolean"

  (Aeson.Number e, Aeson.Number a) -> case (Sci.toBoundedInteger e, Sci.toBoundedInteger a) of
    (Just (ne :: Int64), Just (na :: Int64)) ->
        if ne == na then Success else Failed "Integers differ"
    _ -> case (Sci.toBoundedInteger e, Sci.toBoundedInteger a) of
      (Just (ne :: Word64), Just (na :: Word64)) ->
          if ne == na then Success else Failed "Integers differ"
      _ -> case (Sci.toBoundedRealFloat e, Sci.toBoundedRealFloat a) of
        (Right de, Right da) ->
          if abs(de - da) <= eps then Success else Failed $ "Doubles difference is " <> tshow (abs (de - da))
        _ -> Failed $ tshow e <> " != " <> tshow a
  (Aeson.Number _, _) -> Failed "Expected a number"


compareLines :: Double -> Text -> Text -> ComparisonResult
compareLines eps expectedLine actualLine = case () of
    _ | not (null errors) -> Failed $ T.concat ["Token ", tshow numToken, ": ", err]
      | length actual == length expected -> Success
      | otherwise   ->  Failed $ T.concat ["Expected ", tshow (length expected), " token(s)"]
  where
    expected = T.words expectedLine
    actual = T.words actualLine
    tokenComparison = zipWith (compareTokens eps) expected actual
    errors = [e | e@(_, Failed _) <- zip [1::Int ..] tokenComparison]
    (numToken, Failed err) = head errors

compareTokens :: Double -> Text -> Text -> ComparisonResult
compareTokens eps expected actual = case () of
    _ | expected == actual -> Success
      | areEqualDoubles eps expected actual -> Success
      | otherwise -> Failed $ T.concat ["Expected ", expected, ", found ", actual]

areEqualDoubles :: Double -> Text -> Text -> Bool
areEqualDoubles eps expected actual = isRight expectedParsed && isRight actualParsed &&
    T.null expectedRest && T.null actualRest && abs (expectedDouble - actualDouble) <= eps
  where
    expectedParsed :: Either String (Double, Text)
    expectedParsed = TextRead.double expected
    Right (expectedDouble, expectedRest) = expectedParsed
    actualParsed = TextRead.double actual
    Right (actualDouble, actualRest) = actualParsed

