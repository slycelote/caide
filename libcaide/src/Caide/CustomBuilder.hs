{-# LANGUAGE CPP, OverloadedStrings #-}
module Caide.CustomBuilder(
      createBuilderFromDirectory
) where

import Prelude hiding (FilePath)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
-- TODO: use a safe exceptions library for correct handling of async exceptions
import qualified Control.Exception as Exc
import Control.Monad (filterM, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (Handle, IOMode(ReadMode, WriteMode), withFile)
import System.Process (CreateProcess(create_group, cwd, std_in, std_out), ProcessHandle,
    StdStream(UseHandle), interruptProcessGroupOf, proc, waitForProcess, withCreateProcess)
import System.Exit (ExitCode(..))
import System.Timeout (timeout)

import Filesystem.Path.CurrentOS (FilePath, (</>), (<.>), addExtension, replaceExtension)
import qualified Filesystem.Path.CurrentOS as FS
import Filesystem (writeTextFile)
import Filesystem.Util (isExecutableFile)
import qualified Filesystem.Util as FS

import Caide.Logger (logInfo, logWarn, logError)
import Caide.Types (BuilderResult(BuildFailed, NoEvalTests), Builder, ProblemID)
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
#if mingw32_HOST_OS
    let candidates = [addExtension name "bat", addExtension name "exe"]
#else
    let candidates = [addExtension name "sh", name]
#endif
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

createBuilderFromDirectory :: MonadIO m => FilePath -> [CreateBuilderOption] -> m (Either Text Builder)
createBuilderFromDirectory dirPath optionModifiers = do
    let options = combine optionModifiers
    mbBuildExe <- findExecutable dirPath "build"
    case mbBuildExe of
        Just buildExe -> return . Right $ createBuilderFromBuildExe dirPath buildExe options
        Nothing -> do
            mbRunExe <- findExecutable dirPath "run"
            return $ case mbRunExe of
                Just runExe -> Right $ createBuilderFromRunExe dirPath runExe options
                Nothing -> Left $ T.concat ["Neither `build' nor `run' executables found in ", FS.pathToText dirPath]

executeTest :: MonadIO m => FilePath -> FilePath -> FilePath -> m ComparisonResult
executeTest dirPath runExe inFile = liftIO $
    withFile fullInPath ReadMode $ \hin ->
    withFile fullOutPath WriteMode $ \hout -> do
        logInfo $ "Testing " <> FS.pathToText inFile <> "..."
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
  where
    fullInPath  = FS.pathToString $ dirPath </> inFile
    fullOutPath = FS.pathToString $ dirPath </> Paths.testsDir </> replaceExtension inFile "out"

safeExecuteTestWithTimeout :: MonadIO m => Int -> FilePath -> FilePath -> FilePath -> m TestRunResult
safeExecuteTestWithTimeout timeLimitMicroSecs dirPath runExe inFile = liftIO $ do
    res <- Exc.try $ timeout timeLimitMicroSecs $ executeTest dirPath runExe inFile
    -- TODO: Add time
    return $ makeTestRunResult $ case res of
        Right Nothing  -> Failed "Time limit exceeded"
        Left e         -> Error $ "Error while executing the test: " <> T.pack (Exc.displayException (e :: Exc.SomeException))
        Right (Just r) -> r

createBuilderFromRunExe :: MonadIO m => FilePath -> FilePath -> CreateBuilderOptions -> ProblemID -> m BuilderResult
createBuilderFromRunExe dirPath runExe options _probId = liftIO $ do
    testList <- TestCases.updateTestList dirPath
    let testNames = map fst testList
        inFiles = [FS.decodeString (T.unpack testName) <.> "in" | testName <- testNames]
    results <- mapM (safeExecuteTestWithTimeout (runTimeout options) dirPath runExe) inFiles
    let testReport = zip testNames results
        serializedReport = serializeTestReport testReport
    writeTextFile (dirPath </> Paths.testsDir </> Paths.testReportFile) serializedReport
    return NoEvalTests

createBuilderFromBuildExe :: MonadIO m => FilePath -> FilePath -> CreateBuilderOptions -> ProblemID -> m BuilderResult
createBuilderFromBuildExe dirPath buildExe options probId = liftIO $ do
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
                Just runExe -> createBuilderFromRunExe dirPath runExe options probId
                Nothing -> do
                    logError $ "`run' executable not found in " <> FS.pathToText dirPath
                    return BuildFailed

