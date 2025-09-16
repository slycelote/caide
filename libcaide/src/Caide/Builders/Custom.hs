{-# LANGUAGE OverloadedStrings #-}
module Caide.Builders.Custom(
      builder
    , getLegacyCustomBuilderName
) where

import Control.Monad.Extended (throwError, liftIO)
import Data.Maybe (listToMaybe)
import Data.Either (fromRight)
import qualified Data.Text as T
import qualified Data.Text.IO.Util as T

import qualified Filesystem.Path.CurrentOS as FS

import System.Exit (ExitCode(..))
import System.Process (shell, createProcess, waitForProcess, cwd)

import qualified Data.ConfigFile as CF

import qualified Caide.Paths as Paths
import Caide.Types
import Caide.Util (tshow)


readConfigFile :: CaideIO CF.ConfigParser
readConfigFile = do
    root <- caideRoot
    cp <- do
        mbcp <- liftIO $ CF.readfile CF.emptyCP $ FS.encodeString $ Paths.caideConfFile root
        either throwError pure mbcp

    pure $ case CF.set cp "DEFAULT" "caideRoot" $ FS.encodeString root of
        Right conf' -> conf'{ CF.accessfunc = CF.interpolatingAccess 10, CF.usedefault = True }
        _ -> error "Impossible happened: DEFAULT section doesn't exist"

getLegacyCustomBuilderName :: [T.Text] -> CaideIO (Maybe T.Text)
getLegacyCustomBuilderName languageNames = do
    cp <- readConfigFile
    let legacyBuilderExists langName = CF.has_option cp langName "build_and_run_tests"
        buildersExist = map (legacyBuilderExists . T.unpack) languageNames
        legacyBuilderNames = [name | (name, True) <- zip languageNames buildersExist]
    pure $ listToMaybe legacyBuilderNames

-- Legacy "custom builder"
builder :: T.Text -> ProblemID -> CaideIO BuilderResult
builder name probId = do
    root <- caideRoot
    cp <- readConfigFile
    let mbcmd = CF.get cp (T.unpack name) "build_and_run_tests"
        evaluatesTests = fromRight True $ CF.get cp (T.unpack name) "evaluates_tests"
    cmd <- either throwError pure mbcmd

    let probDir = Paths.problemDir root probId
        process = shell cmd

    liftIO $ do
        T.putStrLn $ T.unlines [T.append "Executing custom test runner: " name, T.pack cmd]
        (_, _, _, ph) <- createProcess process { cwd = Just (FS.encodeString probDir) }
        exitCode <- waitForProcess ph
        case exitCode of
            ExitSuccess -> do
                T.putStrLn "Done"
                return $ if evaluatesTests then TestsPassed else NoEvalTests
            ExitFailure code -> do
                T.putStrLn $ T.concat ["Builder exit code ", tshow code]
                return $ if evaluatesTests && code == 0xCA1DE then TestsFailed else BuildFailed

