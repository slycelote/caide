module Caide.Commands.Make (
      cmd
    , cmdUpdateTests
    , updateTests
    --, cmdPrepareSubmission
) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.Except (catchError)
import Control.Monad.State (liftIO)
import Data.List (sortBy)
import Data.Ord (comparing)

import qualified Data.Text as T

import Prelude hiding (FilePath)
import Filesystem (isDirectory, listDirectory, createTree, removeFile, copyFile, writeTextFile)
import Filesystem.Path.CurrentOS (FilePath, decodeString, encodeString,
    hasExtension, replaceExtension, basename, filename, (</>))

import Caide.Configuration (getActiveProblem, readProblemState, readCaideState)
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
    , action = const updateTests
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

withProblem ::  (ProblemID -> FilePath -> CaideM IO a) -> CaideM IO a
withProblem processProblem = do
    probId <- getActiveProblem `catchError` const (throw "No active problem. Generate one with `caide problem`")
    root <- caideRoot
    let problemDir = root </> decodeString probId
    problemExists <- liftIO $ isDirectory problemDir
    if problemExists
    then processProblem probId problemDir
    else throw $ "Problem " ++ probId ++ " doesn't exist"

make :: [String] -> CaideIO ()
make _ = withProblem $ \_ _ -> makeProblem

updateTests :: CaideIO ()
updateTests = withProblem $ \_ problemDir -> do
    hState <- readCaideState
    caideExe <- getProp hState "core" "caide_exe"
    liftIO $ do
        copyTestInputs problemDir

        let testDir = problemDir </> decodeString ".caideproblem" </> decodeString "test"
        updateTestList testDir

        writeTextFile (testDir </> decodeString "caideExe.txt") $ T.pack caideExe

prepareSubmission :: CaideIO ()
prepareSubmission = withProblem $ \probId problemDir -> do
    hProblem <- readProblemState probId
    lang <- getProp hProblem "problem" "language"
    case findLanguage lang of
        Nothing       -> throw $ "Unsupported programming language " ++ lang
        Just language -> inlineCode language problemDir

makeProblem :: CaideIO ()
makeProblem = updateTests >> prepareSubmission

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

