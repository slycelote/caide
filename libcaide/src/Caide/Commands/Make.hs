module Caide.Commands.Make (
      cmd
) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.List (sortBy)
import Data.Ord (comparing)

import qualified Data.Text as T

import Prelude hiding (FilePath)
import Filesystem (isDirectory, listDirectory, createTree, removeFile, copyFile)
import Filesystem.Path.CurrentOS (FilePath, decodeString, encodeString, hasExtension, replaceExtension,
                                  basename, filename, (</>))

import Caide.Configuration (getActiveProblem, readProblemConfig, getProblemOption, getProblemConfigFileInDir)
import Caide.Registry (findLanguage)
import Caide.Types
import Caide.Util (copyFileToDir)
import Caide.TestCases.Types


cmd :: CommandHandler
cmd = CommandHandler
    { command = "make"
    , description = "Generate test program and submission file"
    , usage = "caide make"
    , action = make
    }

make :: CaideEnvironment -> [String] -> IO ()
make env _ = do
    probId <- getActiveProblem env
    if null probId
        then putStrLn "No active problem. Generate one with `caide problem`"
        else do
            let problemDir = getRootDirectory env </> decodeString probId
            problemExists <- isDirectory problemDir
            if problemExists
                then makeProblem env problemDir
                else putStrLn $ "Problem " ++ probId ++ " doesn't exist"


makeProblem :: CaideEnvironment -> FilePath -> IO ()
makeProblem env problemDir = do
    conf <- readProblemConfig $ getProblemConfigFileInDir problemDir
    case findLanguage $ getProblemOption conf "problem" "language" of
        Nothing -> putStrLn "Couldn't determine language for the problem"
        Just lang -> do
            generateTestProgram lang env problemDir
            inlineCode lang env problemDir
            copyTestInputs problemDir
            updateTestList $ problemDir </> decodeString ".caideproblem" </> decodeString "test"

copyTestInputs :: FilePath -> IO ()
copyTestInputs problemDir = do
    let tempTestDir = problemDir </> decodeString ".caideproblem" </> decodeString "test"
    createTree tempTestDir

    -- Cleanup output from previous test run; leave only testList.txt file
    filesToClear <- filter ((`notElem` ["testList.txt", "report.txt"]) . encodeString . filename) <$> listDirectory tempTestDir
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
