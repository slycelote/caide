{-# LANGUAGE OverloadedStrings #-}
module Caide.CaideMain(
    caideMain
) where

import Data.Maybe (isJust, isNothing, fromMaybe)

import Filesystem (getWorkingDirectory, isDirectory)
import qualified Filesystem.Path as F
import Filesystem.Path.CurrentOS (encodeString, parent, (</>))
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))

import Caide.Commands (runMain)


findRootCaideDir :: F.FilePath -> IO (Maybe F.FilePath)
findRootCaideDir curDir | curDir == parent curDir  = return Nothing
findRootCaideDir curDir = do
    let caideDir = curDir </> ".caide"
    rootIsHere <- isDirectory caideDir
    if rootIsHere
    then return $ Just curDir
    else findRootCaideDir $ parent curDir


caideMain :: IO ()
caideMain = do
    args <- getArgs
    let (cmd:_) = args
        mainAction = runMain args
    case mainAction of
        Left ioAction -> ioAction
        Right caideCmd -> do
            workDir  <- getWorkingDirectory
            caideDir <- findRootCaideDir workDir
            case () of
              _ | cmd /= "init" && isNothing caideDir -> do
                    putStrLn $ encodeString workDir ++ " is not a valid caide directory"
                    halt
                | cmd == "init" && isJust caideDir    -> do
                    putStrLn "Caide directory already initialized"
                    halt
                -- special case for init command: pass working directory as the first parameter
                | otherwise -> caideCmd $ fromMaybe workDir caideDir

halt :: IO ()
halt = exitWith $ ExitFailure 0xCA1DE


