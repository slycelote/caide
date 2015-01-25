{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text.IO as T

import Filesystem (getWorkingDirectory, isDirectory)
import qualified Filesystem.Path as F
import Filesystem.Path.CurrentOS (decodeString, encodeString, parent, (</>))
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))

import Caide.Configuration (describeError)
import Caide.Types
import Caide.Commands (runMain)


findRootCaideDir :: F.FilePath -> IO (Maybe F.FilePath)
findRootCaideDir curDir | curDir == parent curDir  = return Nothing
findRootCaideDir curDir = do
    let caideDir = curDir </> decodeString ".caide"
    rootIsHere <- isDirectory caideDir
    if rootIsHere
    then return $ Just curDir
    else findRootCaideDir $ parent curDir


printUsage :: IO ()
printUsage = T.putStrLn "Usage: caide <cmd> ..."

main :: IO ()
main = do
    args <- getArgs
    let (cmd:_) = args
        mainAction = runMain args
    case mainAction of
        Left ioAction -> ioAction
        Right caideCmd -> do
            workDir <- getWorkingDirectory
            when (null args) $ printUsage >> halt
            caideDir <- findRootCaideDir workDir
            case () of
              _ | cmd /= "init" && isNothing caideDir -> do
                    putStrLn $ encodeString workDir ++ " is not a valid caide directory"
                    halt
                | cmd == "init" && isJust caideDir    -> do
                    putStrLn "Caide directory already initialized"
                    halt
                -- special case for init command: pass working directory as the first parameter
                | otherwise -> runAction caideCmd $ fromMaybe workDir caideDir

runAction :: CaideIO () -> F.FilePath -> IO ()
runAction cmd root = do
    ret <- runInDirectory root cmd
    case ret of
        Left err -> putStrLn (describeError err) >> halt
        _        -> return ()

halt :: IO ()
halt = exitWith $ ExitFailure 0xCA1DE

