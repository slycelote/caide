{-# LANGUAGE OverloadedStrings #-}
module Caide.CustomBuilder(
      createBuilderFromDirectory
) where

import Prelude hiding (FilePath)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Control.Exception as Exc
import Control.Monad.Extended (MonadIO, liftIO, filterM, when)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (Handle, IOMode(ReadMode, WriteMode))
import System.Process (CreateProcess(create_group, cwd, std_in, std_out), ProcessHandle,
    StdStream(UseHandle), interruptProcessGroupOf, proc, waitForProcess, withCreateProcess)
import System.Exit (ExitCode(..))
import System.Info (os)
import System.Timeout (timeout)

import Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem as FS
import Filesystem (writeTextFile)
import Filesystem.Util (isExecutableFile)
import qualified Filesystem.Util as FS

import Caide.Logger (logInfo, logWarn, logError)
import Caide.Types.Builder (BuilderResult(BuildFailed, NoEvalTests))
import Caide.Problem (Problem)
import Caide.TestCases.Types (ComparisonResult(Error, Failed, Ran), TestRunResult, makeTestRunResult,
    serializeTestReport)
import qualified Caide.TestCases as TestCases
import qualified Caide.Paths as Paths
import Caide.Util (tshow)


interruptibleWaitForProcess :: ProcessHandle -> IO ExitCode
interruptibleWaitForProcess ph = do
    ec <- newEmptyMVar
    _threadId <- forkIO $ waitForProcess ph >>= putMVar ec
    takeMVar ec

withCreateProcess' :: CreateProcess -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a) -> IO a
withCreateProcess' cp action = withCreateProcess cp{create_group=True} $ \stdin stdout stderr ph ->
    action stdin stdout stderr ph `Exc.finally` interruptProcessGroupOf ph

findExecutable :: MonadIO m => FilePath -> FilePath -> m (Maybe FilePath)
findExecutable dirPath name = do
    let candidates = if os == "mingw32"
        then [FS.addExtension name "bat", FS.addExtension name "exe"]
        else [FS.addExtension name "sh", name]
    candidateFiles <- filterM isExecutableFile [dirPath </> c | c <- candidates]
    when (length candidateFiles > 1) $
        logWarn $ "More than one executable file with name `" <> FS.pathToText name <> "'found"
    return $ listToMaybe candidateFiles

data CreateBuilderOption = BuildTimeout Int
                         | RunTimeout Int

data CreateBuilderOptions = CreateBuilderOptions
                          { buildTimeout :: Int
                          , runTimeout :: Int
                          }

applyModifier :: CreateBuilderOption -> CreateBuilderOptions -> CreateBuilderOptions
applyModifier option oldOptions = case option of
    BuildTimeout t -> oldOptions{buildTimeout=t}
    RunTimeout t -> oldOptions{runTimeout=t}

seconds :: Int
seconds = 1000000

combine :: [CreateBuilderOption] -> CreateBuilderOptions
combine = foldr applyModifier $
    CreateBuilderOptions{buildTimeout=10*seconds, runTimeout=5*seconds}

createBuilderFromDirectory :: (MonadIO m1, MonadIO m2) =>
    FilePath -> Problem -> [CreateBuilderOption] -> m1 (Either Text (m2 BuilderResult))
createBuilderFromDirectory dirPath problem optionModifiers = do
    let options = combine optionModifiers
    mbBuildExe <- findExecutable dirPath "build"
    case mbBuildExe of
        Just buildExe -> pure . Right $ liftIO $ buildAndRun dirPath buildExe problem options
        Nothing -> do
            mbRunExe <- findExecutable dirPath "run"
            case mbRunExe of
                Just runExe -> pure . Right $ liftIO $ run dirPath runExe problem options
                Nothing -> pure . Left $ "Neither `build' nor `run' executables found in " <> FS.pathToText dirPath

executeTest :: MonadIO m => FilePath -> FilePath -> Text -> m ComparisonResult
executeTest dirPath runExe testName = liftIO $
    FS.withFile (dirPath </> Paths.testInput testName) ReadMode $ \hin ->
    FS.withFile (dirPath </> Paths.userTestOutput testName) WriteMode $ \hout -> do
        logInfo $ "Testing " <> testName <> "..."
        let cp = (proc (FS.pathToString $ dirPath </> runExe) []) {
                     cwd = Just $ FS.pathToString dirPath
                   , std_in = UseHandle hin
                   , std_out = UseHandle hout
                   }
        exitCode <- withCreateProcess' cp $ \ _stdin _stdout _stderr hProcess ->
            interruptibleWaitForProcess hProcess
        return $ case exitCode of
            ExitSuccess -> Ran
            ExitFailure intCode -> Failed $ "Test runner exited with error code " <> tshow intCode

safeExecuteTestWithTimeout :: MonadIO m => Int -> FilePath -> FilePath -> Text -> m TestRunResult
safeExecuteTestWithTimeout timeLimitMicroSecs dirPath runExe testName = liftIO $ do
    res <- Exc.try $ timeout timeLimitMicroSecs $ executeTest dirPath runExe testName
    -- TODO: Add time
    return $ makeTestRunResult $ case res of
        Right Nothing  -> Failed "Time limit exceeded"
        Left e         -> Error $ "Error while executing the test: " <> T.pack (Exc.displayException (e :: Exc.SomeException))
        Right (Just r) -> r

run :: FilePath -> FilePath -> Problem -> CreateBuilderOptions -> IO BuilderResult
run dirPath runExe problem options = do
    testList <- TestCases.updateTests dirPath problem
    let testNames = map fst testList
    results <- mapM (safeExecuteTestWithTimeout (runTimeout options) dirPath runExe) testNames
    let testReport = zip testNames results
        serializedReport = serializeTestReport testReport
    writeTextFile (dirPath </> Paths.testReportFile) serializedReport
    return NoEvalTests

buildAndRun :: FilePath -> FilePath -> Problem -> CreateBuilderOptions -> IO BuilderResult
buildAndRun dirPath buildExe problem options = do
    logInfo $ "Building with " <> FS.pathToText buildExe <> "..."
    let cp = (proc (FS.pathToString $ dirPath </> buildExe) []) {
                 cwd = Just $ FS.pathToString dirPath
               }
    exitCode <- Exc.try $ timeout (buildTimeout options) $ withCreateProcess' cp $ \ _stdin _stdout _stderr hProcess ->
        interruptibleWaitForProcess hProcess
    case exitCode of
        Left e -> do
            logError $ "Error while building: " <> T.pack (Exc.displayException (e :: Exc.SomeException))
            return BuildFailed
        Right Nothing -> do
            logError "Build timed out"
            return BuildFailed
        Right (Just (ExitFailure intCode)) -> do
            logError $ "Build exited with error code " <> tshow intCode
            return BuildFailed
        Right (Just ExitSuccess) -> do
            mbRunExe <- findExecutable dirPath "run"
            case mbRunExe of
                Just runExe -> run dirPath runExe problem options
                Nothing -> do
                    logError $ "`run' executable not found in " <> FS.pathToText dirPath
                    return BuildFailed

