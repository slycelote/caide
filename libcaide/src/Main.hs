module Main where

import Control.Monad (forM_)
import Data.List (find)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text as T
import Filesystem (getWorkingDirectory, isDirectory)
import qualified Filesystem.Path as F
import Filesystem.Path.CurrentOS (decodeString, encodeString, parent, (</>))
import System.Environment (getArgs)

import Caide.Codeforces.Parser (codeforcesParser)
import Caide.Types (CommandHandler(..), parse)

import qualified Caide.Commands.BuildScaffold as BuildScaffold
import qualified Caide.Commands.Checkout as Checkout
import qualified Caide.Commands.Init as Init
import qualified Caide.Commands.Make as Make
import qualified Caide.Commands.ParseProblem as ParseProblem



findRootCaideDir :: F.FilePath -> IO (Maybe F.FilePath)
findRootCaideDir curDir | curDir == parent curDir  = return Nothing
findRootCaideDir curDir = do
    let caideDir = curDir </> decodeString ".caide"
    rootIsHere <- isDirectory caideDir
    if rootIsHere
        then return $ Just curDir
        else findRootCaideDir $ parent curDir 


commands :: [CommandHandler]
commands = [Init.cmd, ParseProblem.cmd, BuildScaffold.cmd, Checkout.cmd, Make.cmd]

findCommand :: String -> Maybe CommandHandler
findCommand cmdName = find ((== cmdName) . command) commands

printUsage :: IO ()
printUsage = do
    putStrLn "Usage: caide [cmd] ..."
    forM_ commands $ \cmd ->
        putStrLn $ "  caide " ++ command cmd ++ ": " ++ description cmd

main :: IO ()
main = do
    args <- getArgs
    workDir <- getWorkingDirectory
    if null args
        then printUsage
        else do
            let (cmd:commandArgs) = args
            case findCommand cmd of
                Nothing -> printUsage
                Just c  -> do
                    caideDir <- findRootCaideDir workDir
                    case () of
                      _ | cmd /= "init" && isNothing caideDir -> putStrLn $ encodeString workDir ++ " is not a valid caide directory"
                        | cmd == "init" && isJust caideDir    -> putStrLn "Caide directory already initialized"
                        -- special case for init command: pass working directory as the first parameter
                        | otherwise -> action c (fromMaybe workDir caideDir) commandArgs

test :: IO ()
test = do
    parseResult <- codeforcesParser `parse` T.pack "http://codeforces.com/contest/400/problem/B"
    case parseResult of
        Left err -> putStrLn err
        Right (problem, testCases) -> do
            print problem
            print testCases
