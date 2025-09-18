{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Caide.Commands.CHelperHttpServer(
      runHttpServer
) where

import Control.Arrow ((&&&))
import Control.Concurrent.Async (withAsync)
import qualified Control.Concurrent.Async as Async
import Control.Monad (forM_, void, when)
import Control.Monad.Except (catchError)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Filesystem.Path as F
import qualified Filesystem.Path.CurrentOS as F
import Network.Socket (tupleToHostAddress, withSocketsDo)
import System.IO (hSetBuffering, stdout, stderr, BufferMode(NoBuffering))

import Data.Aeson (FromJSON(parseJSON), eitherDecode', withObject, (.:))
import Network.Shed.Httpd (initServerBind, Request(reqBody), Response(Response))

import Caide.CheckUpdates (checkUpdates)
import Caide.Commands.ParseProblem (saveProblemWithScaffold)
import Caide.GlobalState (activeProblem, modifyGlobalState)
import Caide.Logger (logInfo, logError)
import Caide.Monad (CaideIO, caideSettings, Verbosity(Debug),
    CaideEnv(verbosity), makeCaideEnv, runInDirectory, describeError)
import Caide.Parsers.Common (CHelperProblemParser(chelperParse))
import Caide.Registry (findCHelperProblemParser)
import Caide.Settings (chelperPort, companionPort)
import Caide.Types
import Caide.Util (newDefaultHttpClient)


runHttpServer :: Verbosity -> F.FilePath -> IO ()
runHttpServer v root = do
    httpClient <- newDefaultHttpClient
    let env = makeCaideEnv root v httpClient
    mbPorts <- runInDirectory env getPorts
    case mbPorts of
        Left err -> logError $ describeError err
        Right (companionPort', chelperPort') -> runServers env companionPort' chelperPort'


runServers :: CaideEnv -> Maybe Int -> Maybe Int -> IO ()
runServers env companionPort' chelperPort' = withSocketsDo $ do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    let servers = case (companionPort', chelperPort') of
            (Just _, Just _)   -> "CHelper and Competitive Companion extensions"
            (Nothing, Just _)  -> "CHelper extension"
            (Just _, Nothing)  -> "Competitive Companion extension"
            (Nothing, Nothing) -> ""
        runServer port handler = forM_ port $ \p ->
            initServerBind p (tupleToHostAddress (127,0,0,1)) (handler env)
    if T.null servers
    then logError "Both CHelper and Competitive Companion servers are disabled. Exiting now."
    else withAsync (void $ runInDirectory env $ checkUpdates `catchError` const (pure ())) $ \a1 ->
         withAsync (void $ runServer companionPort' processCompanionRequest) $ \a2 ->
         withAsync (void $ runServer chelperPort' processCHelperRequest) $ \a3 -> do
             logInfo $ "Running HTTP server for " <> servers <> ". Press Return to terminate."
             _ <- getLine
             forM_ [a1, a2, a3] Async.cancel


getPorts :: CaideIO (Maybe Int, Maybe Int)
getPorts = (companionPort &&& chelperPort) <$> caideSettings

data ParsedProblem = Parsed Problem [TestCase]

createProblems :: CaideEnv -> NE.NonEmpty ParsedProblem -> IO ()
createProblems env parsedProblems = do
    ret <- runInDirectory env $ do
        forM_ parsedProblems $ \(Parsed problem testCases) ->
            saveProblemWithScaffold problem testCases
        let Parsed problem _ = NE.head parsedProblems
        modifyGlobalState $ \s -> s{activeProblem = Just (problemId problem)}

    case ret of
        Left err -> logError $ describeError err
        _        -> return ()


ok, badRequest, internalServerError :: Int
ok = 200
badRequest = 400
internalServerError = 500

makeResponse :: Int -> String -> Response
makeResponse code message = Response code [] message


instance FromJSON InputSource where
  parseJSON = withObject "input description" $ \o -> do
    inputType :: String <- o .: "type"
    case inputType of
        "file"  -> FileInput . F.fromText <$> o .: "fileName"
        "regex" -> InputFilePattern <$> o .: "pattern"
        _ -> pure StdIn

instance FromJSON OutputTarget where
  parseJSON = withObject "output description" $ \o -> do
    outputType <- o .: "type"
    if outputType == ("file" :: String) then FileOutput . F.fromText <$> o .: "fileName" else pure StdOut

instance FromJSON TestCase where
  parseJSON = withObject "test case" $ \o ->
    TestCase <$> o .: "input" <*> o .: "output"

instance FromJSON ParsedProblem where
  parseJSON = withObject "problem description" $ \o -> do
    input <- o .: "input"
    output <- o .: "output"
    probName <- o .: "name"
    tests <- o .: "tests"

    languages <- o .: "languages"
    java <- languages .: "java"
    probId <- java .: "taskClass"

    pure $ Parsed (makeProblem probName probId (Stream input output)) tests

processCompanionRequest :: CaideEnv -> Request -> IO Response
processCompanionRequest env request = do
    let body = LBS.fromStrict . encodeUtf8 . T.pack $ reqBody request
        mbParsed = eitherDecode' body :: Either String ParsedProblem
    when (verbosity env >= Debug) $
        putStrLn $ reqBody request
    case mbParsed of
        Left err -> do
            logError $ "Could not parse input JSON: " <> T.pack err
            return $ makeResponse badRequest err
        Right p -> do
            createProblems env $ NE.fromList [p]
            return $ makeResponse ok "OK"


processCHelperRequest :: CaideEnv -> Request -> IO Response
processCHelperRequest env request = do
    let body = T.pack $ reqBody request
        bodyLines = T.lines body
    case bodyLines of
        [] -> do
            logError "Invalid request!"
            return $ makeResponse badRequest "Invalid request!"
        (first:_) | T.strip first == "json" -> return $ makeResponse ok "" -- Processed by Companion server instead
        (first:rest) -> do
            let chid = T.strip first
                page = T.unlines rest
            err <- process chid page env
            case err of
                Nothing -> return $ makeResponse ok "OK"
                Just e  -> do
                    logError e
                    return $ makeResponse internalServerError $ T.unpack e


process :: T.Text -> T.Text -> CaideEnv -> IO (Maybe T.Text)
process chid page env = case findCHelperProblemParser chid of
    Nothing -> return . Just $ "'" <> chid <> "' not supported"
    Just parser -> do
        res <- chelperParse parser page
        case res of
            Left err -> return . Just $ "Error while parsing the problem: " <> err
            Right (problem, testCases) -> do
                createProblems env $ NE.fromList [Parsed problem testCases]
                return Nothing

