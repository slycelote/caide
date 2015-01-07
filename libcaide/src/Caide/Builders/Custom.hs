module Caide.Builders.Custom (
      builder
) where

import Control.Monad.State (liftIO)

import Filesystem.Path ((</>))
import Filesystem.Path.CurrentOS (decodeString, encodeString)

import System.Exit (ExitCode(..))
import System.Process (shell, createProcess, waitForProcess, cwd)

import Caide.Configuration (readCaideConf)
import Caide.Types

builder :: String -> String -> CaideIO BuilderResult
builder name probId = do
    root <- caideRoot
    hConf <- readCaideConf
    cmd <- getProp hConf name "build_and_run_tests"
    evaluatesTests <- getProp hConf name "evaluates_tests"
    let probDir = root </> decodeString probId
        process = shell cmd

    liftIO $ do
        putStrLn $ unlines ["Executing custom test runner: " ++ name, cmd]
        (_, _, _, ph) <- createProcess process { cwd = Just (encodeString probDir) }
        exitCode <- waitForProcess ph
        case exitCode of
            ExitSuccess -> do
                putStrLn "Done"
                return $ if evaluatesTests then TestsPassed else NoEvalTests
            ExitFailure code -> do
                putStrLn $ "Builder exit code " ++ show code
                return $ if evaluatesTests && code == 0xCA1DE then TestsFailed else BuildFailed

