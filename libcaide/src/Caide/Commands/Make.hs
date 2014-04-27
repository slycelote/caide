module Caide.Commands.Make (
      cmd
) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import qualified Data.Text as T

import Prelude hiding (FilePath)
import Filesystem (isDirectory, listDirectory, createTree,
                   isFile, readTextFile, writeTextFile, removeFile, copyFile)
import Filesystem.Path.CurrentOS (FilePath, decodeString, encodeString, hasExtension, replaceExtension,
                                  basename, filename, (</>))

import Caide.Configuration (readRootConf, getActiveProblem, readProblemConf, getOption)
import Caide.Registry (findLanguage)
import Caide.Types
import Caide.Util (copyFileToDir)


cmd :: CommandHandler
cmd = CommandHandler
    { command = "make"
    , description = "Generate test program and submission file"
    , usage = "caide make"
    , action = make
    }

make :: FilePath -> [String] -> IO ()
make caideRoot _ = do
    conf <- readRootConf caideRoot
    let probId = getActiveProblem conf
    if null probId
        then putStrLn "No active problem. Generate one with `caide problem`"
        else do
            let problemDir = caideRoot </> decodeString probId
            problemExists <- isDirectory problemDir
            if problemExists
                then makeProblem problemDir
                else putStrLn $ "Problem " ++ probId ++ " doesn't exist"


makeProblem :: FilePath -> IO ()
makeProblem problemDir = do
    conf <- readProblemConf problemDir
    case findLanguage $ getOption conf "problem" "language" of
        Nothing -> putStrLn "Couldn't determine language for the problem"
        Just lang -> do
            lang `generateTestProgram` problemDir
            lang `inlineCode` problemDir
            copyTestInputs problemDir
            updateTestList $ problemDir </> decodeString ".caideproblem" </> decodeString "test"

copyTestInputs :: FilePath -> IO ()
copyTestInputs problemDir = do
    let tempTestDir = problemDir </> decodeString ".caideproblem" </> decodeString "test"
    createTree tempTestDir

    -- Cleanup output from previous test run; leave only testList.txt file
    filesToClear <- filter ((/= "testList.txt") . encodeString . filename) <$> listDirectory tempTestDir
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
    fileExists <- isFile testListFile
    tests <- if fileExists then readTests testListFile else return []
    inFileNames <- map (encodeString . basename) .
                   filter (`hasExtension` T.pack "in") <$>
                   listDirectory testsDir
    let newTests = filter (`notElem` map fst tests) inFileNames
        updatedTests = filter (\(name, _) -> name `elem` inFileNames) tests
                       ++ zip newTests (repeat "run")
    writeTests updatedTests testListFile

readTests :: FilePath -> IO [(String, String)]
readTests testListFile = do
    testLines <- map (words . T.unpack) . T.lines <$> readTextFile testListFile
    let toTest [name, state] = (name, state)
        toTest _ = error "Corrupted testList file"
    return $ map toTest testLines

writeTests :: [(String, String)] -> FilePath -> IO ()
writeTests tests testListFile = writeTextFile testListFile text
    where text = T.pack . unlines . map (\(name, state) -> name ++ " " ++ state) $ tests
