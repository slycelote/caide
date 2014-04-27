module Caide.Builders.Custom (
      builder
) where

import Filesystem.Path ((</>))
import Filesystem.Path.CurrentOS (decodeString, encodeString)

import System.Exit (ExitCode(..))
import System.Process (shell, createProcess, waitForProcess, cwd)

import Caide.Configuration (getOption, readRootConf)
import Caide.Types

builder :: String -> Builder
builder name caideRoot probId = do
    conf <- readRootConf caideRoot
    let probDir = caideRoot </> decodeString probId
        cmd = getOption conf name "build_and_run_tests"
        process = shell cmd
    putStrLn $ unlines ["Executing custom test runner:" ++ name, cmd]
    (_, _, _, ph) <- createProcess process { cwd = Just (encodeString probDir) }
    exitCode <- waitForProcess ph
    case exitCode of
        ExitSuccess -> putStrLn "Done" >> return True
        ExitFailure code -> do
            putStrLn $ "Process terminated with exit code " ++ show code
            return False
