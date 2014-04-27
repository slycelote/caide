module Caide.Commands.Make (
      cmd
) where

import Control.Applicative ((<$>))
import qualified Data.Text as T

import Filesystem (isDirectory, isFile, listDirectory, readTextFile, writeTextFile, createTree)
import Filesystem.Path.CurrentOS (decodeString, encodeString, hasExtension, basename, (</>))
import qualified Filesystem.Path as F

import Caide.Configuration (readRootConf, getActiveProblem, readProblemConf, getOption)
import Caide.Registry (findLanguage)
import Caide.Types


cmd :: CommandHandler
cmd = CommandHandler
    { command = "make"
    , description = "Generate test program and submission file"
    , usage = "caide make"
    , action = make
    }

make :: F.FilePath -> [String] -> IO ()
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


makeProblem :: F.FilePath -> IO ()
makeProblem problemDir = do
    conf <- readProblemConf problemDir
    case findLanguage $ getOption conf "problem" "language" of
        Nothing -> putStrLn "Couldn't determine language for the problem"
        Just lang -> do
            lang `generateTestProgram` problemDir
            lang `inlineCode` problemDir
            updateTestList problemDir

-- | Updates testList.txt file:
--    * removes missing tests
--    * adds new tests
--    * makes sure previously failed tests (if any) come first
updateTestList :: F.FilePath -> IO ()
updateTestList problemDir = do
    let problemSettingsDir = problemDir </> decodeString ".caideproblem"
        testListFile = problemSettingsDir </> decodeString "testList.txt"
    createTree problemSettingsDir
    fileExists <- isFile testListFile
    tests <- if fileExists then readTests testListFile else return []
    inFileNames <- map (encodeString . basename) .
                   filter (`hasExtension` T.pack "in") <$>
                   listDirectory problemDir
    let newTests = filter (`notElem` map fst tests) inFileNames
        updatedTests = filter (\(name, _) -> name `elem` inFileNames) tests
                       ++ zip newTests (repeat "run")
    writeTests updatedTests testListFile

readTests :: F.FilePath -> IO [(String, String)]
readTests testListFile = do
    testLines <- map (words . T.unpack) . T.lines <$> readTextFile testListFile
    let toTest [name, state] = (name, state)
        toTest _ = error "Corrupted testList file"
    return $ map toTest testLines

writeTests :: [(String, String)] -> F.FilePath -> IO ()
writeTests tests testListFile = writeTextFile testListFile text
    where text = T.pack . unlines . map (\(name, state) -> name ++ " " ++ state) $ tests
