{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List (isSuffixOf)
import Filesystem (getWorkingDirectory, isDirectory)
import qualified Filesystem.Path as F
import Filesystem.Path.CurrentOS (encodeString, parent, (</>))
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hIsTerminalDevice, stdin)

import Caide.Commands (runMain)
import Caide.Commands.FirefoxServer (runFirefoxServer)


findRootCaideDir :: F.FilePath -> IO (Maybe F.FilePath)
findRootCaideDir curDir | curDir == parent curDir  = return Nothing
findRootCaideDir curDir = do
    let caideDir = curDir </> ".caide"
    rootIsHere <- isDirectory caideDir
    if rootIsHere
    then return $ Just curDir
    else findRootCaideDir $ parent curDir

data RunningMode
    = ExistingDir F.FilePath
    | NewDir F.FilePath
    | FirefoxServer
    deriving (Eq, Show)

findRunningMode :: [String] -> IO RunningMode
findRunningMode [arg] | ".json" `isSuffixOf` arg = do
    -- TODO: Verify that the argument is a path to an app manifest.
    isTerminal <- hIsTerminalDevice stdin
    if isTerminal
        then findRunningMode []
        else return FirefoxServer


findRunningMode _ = do
    workDir <- getWorkingDirectory
    caideDir <- findRootCaideDir workDir
    return $ case caideDir of
        Nothing  -> NewDir workDir
        Just dir -> ExistingDir dir


main :: IO ()
main = do
    args <- getArgs
    let (cmd:_) = args
        mainAction = runMain args
    mode <- findRunningMode args
    case (mode, mainAction) of
        (FirefoxServer, _) -> runFirefoxServer
        (_, Left ioAction) -> ioAction
        (NewDir dir, Right caideCmd) ->
            if cmd == "init"
                then caideCmd dir
                else do
                    putStrLn $ encodeString dir ++ " is not a valid caide directory"
                    halt
        (ExistingDir dir, Right caideCmd) ->
            if cmd == "init"
                then do
                    putStrLn "Caide directory is already initialized"
                    halt
                else caideCmd dir

halt :: IO ()
halt = exitWith $ ExitFailure 0xCA1DE

