module Caide.Builders.Custom (
      builder
) where

import Filesystem.Path ((</>))
import Filesystem.Path.CurrentOS (decodeString, encodeString)

import System.Exit (ExitCode(..))
import System.Process (shell, createProcess, waitForProcess, cwd)

import Caide.Types

builder :: String -> Builder
builder name env probId = do
    cmd <- getUserOption env name "build_and_run_tests"
    let caideRoot = getRootDirectory env
        probDir = caideRoot </> decodeString probId
        process = shell cmd

    putStrLn $ unlines ["Executing custom test runner: " ++ name, cmd]
    (_, _, _, ph) <- createProcess process { cwd = Just (encodeString probDir) }
    exitCode <- waitForProcess ph
    case exitCode of
        ExitSuccess -> putStrLn "Done" >> return True
        ExitFailure code -> do
            putStrLn $ "Process terminated with exit code " ++ show code
            return False
