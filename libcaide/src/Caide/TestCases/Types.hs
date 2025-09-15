{-# LANGUAGE OverloadedStrings #-}

module Caide.TestCases.Types (
      ComparisonResult (..)
    , isError
    , isFailure
    , isSuccessful

    , TestRunResult(..)
    , makeTestRunResult

    , TestReport
    , humanReadableReport
    , humanReadableSummary
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

import Prelude hiding (FilePath)
import Data.Char (isSpace)
import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Parsec as Parsec
import Text.Read (readMaybe)

import Data.Time.Clock (DiffTime, diffTimeToPicoseconds, picosecondsToDiffTime)
import Filesystem.Path.CurrentOS (FilePath)
import Filesystem (isFile, readTextFile, writeTextFile)
import Caide.Util (tshow)


data ComparisonResult = Success        -- ^ Test passed
                      | Ran            -- ^ Test ran but hasn't been evaluated
                      | EtalonUnknown  -- ^ Test ran but etalon is unknown
                      | Skipped        -- ^ Test was skipped
                      | Failed Text    -- ^ Test failed
                      | Error Text     -- ^ Error (e.g. exception in checker)
                      deriving (Show, Eq)

getErrorMessage :: ComparisonResult -> Maybe Text
getErrorMessage (Failed e) = Just e
getErrorMessage (Error e) = Just e
getErrorMessage _ = Nothing

isError :: ComparisonResult -> Bool
isError (Error _) = True
isError _ = False

isFailure :: ComparisonResult -> Bool
isFailure (Failed _) = True
isFailure _ = False

isSuccessful :: ComparisonResult -> Maybe Bool
isSuccessful (Failed _) = Just False
isSuccessful (Error _) = Just False
isSuccessful Success = Just True
isSuccessful _ = Nothing

humanReadable :: ComparisonResult -> Text
humanReadable Success       = "     OK"
humanReadable Ran           = "UNKNOWN"
humanReadable Skipped       = "   SKIP"
humanReadable (Failed _)    = " FAILED"
humanReadable EtalonUnknown = "UNKNOWN"
humanReadable (Error _)     = "  ERROR"

machineReadable :: ComparisonResult -> Text
machineReadable Success = "OK"
machineReadable Ran = "ran"
machineReadable EtalonUnknown = "unknown"
machineReadable Skipped = "skipped"
machineReadable (Failed message) = "failed " <> message
machineReadable (Error err) = "error " <> err


data TestRunResult = TestRunResult
    { testRunStatus :: ComparisonResult
    , testRunTime   :: Maybe DiffTime
    } deriving (Eq, Show)

type TestReport = [(Text, TestRunResult)]

makeTestRunResult :: ComparisonResult -> TestRunResult
makeTestRunResult cmp = TestRunResult cmp Nothing

serializeTestReport :: TestReport -> Text
serializeTestReport = T.unlines .
            map (\(testName, res) -> testName <> " " <> serializeTestRunResult res)

humanReadableTime :: Maybe DiffTime -> Text
humanReadableTime Nothing = ""
humanReadableTime (Just t) = " (" <> tshow (diffTimeToPicoseconds t `div` picosecondsInMs) <> "ms)"

humanReadableReport :: TestReport -> Text
humanReadableReport = T.intercalate "\n" .
            map (\(testName, res) -> testName <> " " <>
                    humanReadable (testRunStatus res) <>
                    humanReadableTime (testRunTime res) <>
                    maybe "" (T.append ": ") (getErrorMessage (testRunStatus res)))

humanReadableSummary :: TestReport -> Text
humanReadableSummary = T.unlines . map toText . NE.group . sort . map (fromComparisonResult . testRunStatus . snd)
    where toText list = T.concat [NE.head list, "\t", tshow (NE.length list)]
          fromComparisonResult (Error _) = "Error"
          fromComparisonResult (Failed _) = "Failed"
          fromComparisonResult r = tshow r

picosecondsInMs :: Integer
picosecondsInMs = 10^(9::Int)

serializeTestRunResult :: TestRunResult -> Text
serializeTestRunResult (TestRunResult status time) = serializedTime <> machineReadable status
  where
    serializedTime = case time of
        Nothing -> ""
        Just t  -> "#time:" <> tshow (diffTimeToPicoseconds t `div` picosecondsInMs) <> "ms "


readComparisonResult :: T.Text -> T.Text -> ComparisonResult
readComparisonResult "OK" _ = Success
readComparisonResult "ran" _ = Ran
readComparisonResult "skipped" _ = Skipped
readComparisonResult "unknown" _ = EtalonUnknown
readComparisonResult "failed" err = Failed err
readComparisonResult "error" err = Error err
readComparisonResult _ _ = Error "Corrupted report file"

deserializeTestReport :: Text -> TestReport
deserializeTestReport text = map parseTest reportLines
  where
    reportLines = filter (not . T.null) $ map T.strip $ T.lines text

    parseTest :: Text -> (Text, TestRunResult)
    parseTest line = either (error . show) id $ Parsec.parse testParser "" line

    testParser = do
        testName <- token
        runResult <- Parsec.optionMaybe $ do
            additionalInfo <- Map.fromList <$> additionalInfoParser
            skipSpace1
            status <- token
            errorMessage <- skipSpace *> (T.pack <$> Parsec.many Parsec.anyChar) <* Parsec.eof
            let res = makeTestRunResult $ readComparisonResult status errorMessage
                time = parseTime =<< Map.lookup "time" additionalInfo
            pure $ res{testRunTime = time}

        return (testName, fromMaybe (makeTestRunResult $ Error "Corrupted report file") runResult)

    additionalInfoParser = Parsec.many $ Parsec.try $ do
        skipSpace1
        _ <- Parsec.char '#'
        key <- T.pack <$> Parsec.many1 (Parsec.satisfy $ \c -> c /= ':' && not (isSpace c))
        _ <- Parsec.char ':'
        value <- token
        pure (key, value)

    parseTime s = res
      where
        s' = fromMaybe s $ T.stripSuffix "ms" s
        mbNum = readMaybe $ T.unpack s'
        res = case mbNum of
            Nothing  -> Nothing
            Just num -> Just $ picosecondsToDiffTime $ num * picosecondsInMs

    skipSpace1 = Parsec.skipMany1 Parsec.space
    skipSpace = Parsec.skipMany Parsec.space
    token = T.pack <$> Parsec.many1 nonSpace
    nonSpace = Parsec.satisfy $ not.isSpace

readTestReport :: FilePath -> IO TestReport
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
    toTest _ = error "Corrupted testList file" -- FIXME

readTests :: FilePath -> IO TestList
readTests testListFile = do
    testListExists <- isFile testListFile
    if testListExists
    then deserializeTestList <$> readTextFile testListFile
    else return []

writeTests :: TestList -> FilePath -> IO ()
writeTests tests testListFile = writeTextFile testListFile $ serializeTestList tests

