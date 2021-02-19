{-# LANGUAGE OverloadedStrings #-}

module Caide.TestCases.Types (
      ComparisonResult (..)
    , isSuccessful
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

import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Parsec as Parsec

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

isSuccessful :: ComparisonResult a -> Maybe Bool
isSuccessful (Failed _) = Just False
isSuccessful (Error _) = Just False
isSuccessful Success = Just True
isSuccessful _ = Nothing

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

readComparisonResult :: T.Text -> Maybe T.Text -> ComparisonResult Text
readComparisonResult "OK" _ = Success
readComparisonResult "ran" _ = Ran
readComparisonResult "skipped" _ = Skipped
readComparisonResult "unknown" _ = EtalonUnknown
readComparisonResult "failed" err = Failed $ fromMaybe "" err
readComparisonResult "error" err = Error $ fromMaybe "" err
readComparisonResult _ _ = Error "Corrupted report file"

deserializeTestReport :: Text -> TestReport Text
deserializeTestReport text = map parseTest reportLines
  where
    reportLines = filter (not . T.null) $ map T.strip $ T.lines text

    parseTest :: Text -> (Text, ComparisonResult Text)
    parseTest line = fromRight (error "Impossible happened") $
        Parsec.parse parser "" line

    parser = do
        testName <- token
        rest <- Parsec.optionMaybe $ do
            skipSpace1
            status <- token
            errorMessage <- Parsec.optionMaybe $ do
                skipSpace1
                msg <- T.pack <$> Parsec.many1 Parsec.anyToken
                Parsec.eof
                pure msg
            pure $ readComparisonResult status errorMessage

        return (testName, fromMaybe (Error "Corrupted report file") rest)

    skipSpace1 = Parsec.skipMany1 Parsec.space
    token = T.pack <$> Parsec.many1 nonSpace
    nonSpace = Parsec.satisfy $ not.isSpace

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

