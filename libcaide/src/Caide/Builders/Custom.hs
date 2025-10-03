{-# LANGUAGE OverloadedStrings #-}
module Caide.Builders.Custom(
      builder
    , getLegacyCustomBuilderName
) where

import Control.Monad.Extended (liftIO)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as T

import qualified Filesystem.Path.CurrentOS as FS

import System.Exit (ExitCode(..))
import System.Process (shell, createProcess, waitForProcess, cwd)

import qualified Data.ConfigFile as CF

import Caide.Configuration (readConfigFile, getProp, getPropOrDefault)
import qualified Caide.Paths as Paths
import Caide.Monad (CaideIO, caideRoot, orThrow, rightOrThrow)
import Caide.Types (ProblemID)
import Caide.Types.Builder (BuilderResult(..))
import Caide.Util (tshow)


readConfigFile' :: CaideIO CF.ConfigParser
readConfigFile' = do
    root <- caideRoot
    mbcp <- liftIO $ readConfigFile root "caide.ini"
    mbcp `orThrow` \e ->
        "Failed to read caide.ini: " <> e

getLegacyCustomBuilderName :: [T.Text] -> CaideIO (Maybe T.Text)
getLegacyCustomBuilderName languageNames = do
    cp <- readConfigFile'
    let legacyBuilderExists langName = CF.has_option cp langName "build_and_run_tests"
        buildersExist = map (legacyBuilderExists . T.unpack) languageNames
        legacyBuilderNames = [name | (name, True) <- zip languageNames buildersExist]
    pure $ listToMaybe legacyBuilderNames

-- Legacy "custom builder"
builder :: T.Text -> ProblemID -> CaideIO BuilderResult
builder name probId = do
    root <- caideRoot
    cp <- readConfigFile'
    cmd <- rightOrThrow $ getProp cp (T.unpack name) "build_and_run_tests"
    evaluatesTests <- rightOrThrow $ getPropOrDefault cp (T.unpack name) "evaluates_tests" True
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

