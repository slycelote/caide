module Caide.Commands.Make (
      cmd
    , cmdUpdateTests
    , updateTests
    --, cmdPrepareSubmission
) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.List (sortBy)
import Data.Maybe (fromMaybe, isNothing)
import Data.Ord (comparing)

import qualified Data.Text as T

import Prelude hiding (FilePath)
import Filesystem (isDirectory, listDirectory, createTree, removeFile, copyFile, writeTextFile)
import Filesystem.Path.CurrentOS (FilePath, decodeString, encodeString,
    hasExtension, replaceExtension, basename, filename, (</>))

import Caide.Configuration (getActiveProblem, readProblemConfig, getProblemOption, getProblemConfigFileInDir)
import Caide.Registry (findLanguage)
import Caide.Types
import Caide.Util (copyFileToDir)
import Caide.TestCases.Types


cmd :: CommandHandler
cmd = CommandHandler
    { command = "make"
    , description = "Prepares submission file and updates test list"
    , usage = "caide make"
    , action = make
    }

cmdUpdateTests :: CommandHandler
cmdUpdateTests = CommandHandler
    { command = "update_tests"
    , description = "(Internal) Updates test list"
    , usage = "caide update_tests"
    , action = \env _ -> updateTests env
    }

{-
cmdPrepareSubmission :: CommandHandler
cmdPrepareSubmission = CommandHandler
    { command = "prepare_submission"
    , description = "(Internal) Prepares code for submission"
    , usage = "caide prepare_submission"
    , action = \env _ -> prepareSubmission env
    }
-}

make :: CaideEnvironment -> [String] -> IO (Maybe String)
make env _ = do
    probId <- getActiveProblem env
    if null probId
        then return . Just $ "No active problem. Generate one with `caide problem`"
        else do
            let problemDir = getRootDirectory env </> decodeString probId
            problemExists <- isDirectory problemDir
            if problemExists
                then makeProblem env
                else return . Just $ "Problem " ++ probId ++ " doesn't exist"

updateTests :: CaideEnvironment -> IO (Maybe String)
updateTests env = do
    probId <- getActiveProblem env
    if null probId
        then return . Just $ "No active problem. Generate one with `caide problem`"
        else do
            let problemDir = getRootDirectory env </> decodeString probId
            problemExists <- isDirectory problemDir
            if problemExists
                then do
                    copyTestInputs problemDir

                    let testDir = problemDir </> decodeString ".caideproblem" </> decodeString "test"
                    updateTestList testDir

                    caideExe <- getInternalOption env "core" "caide_exe"
                    writeTextFile (testDir </> decodeString "caideExe.txt") $ T.pack caideExe

                    return Nothing

                else return . Just $ "Problem " ++ probId ++ " doesn't exist"

prepareSubmission :: CaideEnvironment -> IO (Maybe String)
prepareSubmission env = do
    probId <- getActiveProblem env
    if null probId
        then return . Just $ "No active problem. Generate one with `caide problem`"
        else do
            let problemDir = getRootDirectory env </> decodeString probId
            problemExists <- isDirectory problemDir
            if problemExists
                then do
                    conf <- readProblemConfig $ getProblemConfigFileInDir problemDir
                    case findLanguage $ getProblemOption conf "problem" "language" of
                        Nothing -> return . Just $ "Couldn't determine language for the problem"
                        Just lang -> do
                            inlineCode lang env problemDir
                            return Nothing

                else return . Just $ "Problem " ++ probId ++ " doesn't exist"

makeProblem :: CaideEnvironment -> IO (Maybe String)
makeProblem env = do
    ret1 <- prepareSubmission env
    ret2 <- updateTests env
    if isNothing ret1 && isNothing ret2
    then return Nothing
    else return . Just $ fromMaybe "" ret1 ++ " " ++ fromMaybe "" ret2

copyTestInputs :: FilePath -> IO ()
copyTestInputs problemDir = do
    let tempTestDir = problemDir </> decodeString ".caideproblem" </> decodeString "test"
    createTree tempTestDir

    -- Cleanup output from previous test run
    let filesToKeep = ["testList.txt", "report.txt", "caideExe.txt"]
    filesToClear <- filter ((`notElem` filesToKeep) . encodeString . filename) <$> listDirectory tempTestDir
    forM_ filesToClear removeFile

    fileList <- listDirectory problemDir
    -- Copy input files
    let testInputs = filter (`hasExtension` T.pack "in") fileList
    forM_ testInputs $ \inFile -> copyFileToDir inFile tempTestDir

    -- Copy output files
    let testEtalons = filter (`hasExtension` T.pack "out") fileList
        outPathToEtalonPath etalonFile = tempTestDir </> replaceExtension (filename etalonFile) (T.pack "etalon")
    forM_ testEtalons $ \etalonFile -> copyFile etalonFile $ outPathToEtalonPath etalonFile




-- | Updates testList.txt file:
--    * removes missing tests
--    * adds new tests
--    * makes sure previously failed tests (if any) come first
updateTestList :: FilePath -> IO ()
updateTestList testsDir = do
    let testListFile = testsDir </> decodeString "testList.txt"
        previousRunFile = testsDir </> decodeString "report.txt"
    tests <- readTests testListFile
    inFileNames <- map (encodeString . basename) .
                   filter (`hasExtension` T.pack "in") <$>
                   listDirectory testsDir
    report <- readTestReport previousRunFile
    let newTestNames = filter (`notElem` map fst tests) inFileNames
        updatedTests = filter (\(name, _) -> name `elem` inFileNames) tests
                       ++ zip newTestNames (repeat Run)
        succeededAndName (testName, _) = case lookup testName report of
            Just (Error ()) -> (False, testName)
            _               -> (True, testName)
        sortedTests = sortBy (comparing succeededAndName) updatedTests
    writeTests sortedTests testListFile

