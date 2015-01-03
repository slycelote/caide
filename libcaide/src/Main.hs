module Main where

import Control.Monad (forM_)
import Data.List (find)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Filesystem (getWorkingDirectory, isDirectory)
import qualified Filesystem.Path as F
import Filesystem.Path.CurrentOS (decodeString, encodeString, parent, (</>))
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))

import Caide.Configuration (describeError)
import Caide.Types

import qualified Caide.Commands.Archive as Archive
import qualified Caide.Commands.BuildScaffold as BuildScaffold
import qualified Caide.Commands.Checkout as Checkout
import qualified Caide.Commands.GetOpt as GetOpt
import qualified Caide.Commands.Init as Init
import qualified Caide.Commands.Make as Make
import qualified Caide.Commands.ParseContest as ParseContest
import qualified Caide.Commands.ParseProblem as ParseProblem
import qualified Caide.Commands.RunTests as RunTests



findRootCaideDir :: F.FilePath -> IO (Maybe F.FilePath)
findRootCaideDir curDir | curDir == parent curDir  = return Nothing
findRootCaideDir curDir = do
    let caideDir = curDir </> decodeString ".caide"
    rootIsHere <- isDirectory caideDir
    if rootIsHere
    then return $ Just curDir
    else findRootCaideDir $ parent curDir


commands :: [CommandHandler]
commands = [Init.cmd, ParseProblem.cmd, BuildScaffold.cmd, Checkout.cmd, Make.cmd, RunTests.cmd,
            Archive.cmd, ParseContest.cmd,
            -- Commands for internal use
            GetOpt.cmd, GetOpt.cmdState, GetOpt.cmdProblem, GetOpt.cmdProblemState, RunTests.cmdEvaluate,
            Make.cmdUpdateTests]

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
    let (cmd:commandArgs) = args
    workDir <- getWorkingDirectory
    if null args
        then printUsage >> halt
        else case findCommand cmd of
            Nothing -> printUsage >> halt
            Just c  -> do
                caideDir <- findRootCaideDir workDir
                case () of
                  _ | cmd /= "init" && isNothing caideDir -> do
                        putStrLn $ encodeString workDir ++ " is not a valid caide directory"
                        halt
                    | cmd == "init" && isJust caideDir    -> do
                        putStrLn "Caide directory already initialized"
                        halt
                    -- special case for init command: pass working directory as the first parameter
                    | otherwise -> runAction c (fromMaybe workDir caideDir) commandArgs

runAction :: CommandHandler -> F.FilePath -> [String] -> IO ()
runAction cmd root args = do
    ret <- runInDirectory root $ action cmd args
    case ret of
        Left err -> putStrLn (describeError err) >> halt
        _        -> return ()

halt :: IO ()
halt = exitWith (ExitFailure 0xCA1DE)

