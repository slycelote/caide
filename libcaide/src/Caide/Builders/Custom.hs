{-# LANGUAGE OverloadedStrings #-}
module Caide.Builders.Custom(
      builder
) where

import Control.Monad.State (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO.Util as T

import qualified Filesystem.Path.CurrentOS as FS

import System.Exit (ExitCode(..))
import System.Process (shell, createProcess, waitForProcess, cwd)

import Caide.Configuration (readCaideConf, withDefault)
import qualified Caide.Paths as Paths
import Caide.Types
import Caide.Util (tshow)

builder :: T.Text -> ProblemID -> CaideIO BuilderResult
builder name probId = do
    root <- caideRoot
    hConf <- readCaideConf
    cmd <- getProp hConf (T.unpack name) "build_and_run_tests"
    evaluatesTests <- withDefault True $ getProp hConf (T.unpack name) "evaluates_tests"
    let probDir = Paths.problemDir root probId
        process = shell $ T.unpack cmd

    liftIO $ do
        T.putStrLn $ T.unlines [T.append "Executing custom test runner: " name, cmd]
        (_, _, _, ph) <- createProcess process { cwd = Just (FS.encodeString probDir) }
        exitCode <- waitForProcess ph
        case exitCode of
            ExitSuccess -> do
                T.putStrLn "Done"
                return $ if evaluatesTests then TestsPassed else NoEvalTests
            ExitFailure code -> do
                T.putStrLn $ T.concat ["Builder exit code ", tshow code]
                return $ if evaluatesTests && code == 0xCA1DE then TestsFailed else BuildFailed

