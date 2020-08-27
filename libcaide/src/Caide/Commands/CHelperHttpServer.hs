{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Caide.Commands.CHelperHttpServer(
      runHttpServer
) where

#ifndef AMP
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Concurrent.Async (withAsync)
import qualified Control.Concurrent.Async as Async
import Control.Monad (forM_, void, when)
import Control.Monad.Except (catchError)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Filesystem.Path as F
import qualified Filesystem.Path.CurrentOS as F
import Network.Socket (tupleToHostAddress, withSocketsDo)
import System.IO (hSetBuffering, stdout, stderr, BufferMode(NoBuffering))

import Data.Aeson (FromJSON(parseJSON), eitherDecode', withObject, (.:))
import Network.Shed.Httpd (initServerBind, Request(reqBody), Response(Response))

import Caide.CheckUpdates (checkUpdates)
import Caide.Configuration (describeError, orDefault, readCaideConf, setActiveProblem)
import Caide.Commands.ParseProblem (saveProblemWithScaffold)
import Caide.Logger (logInfo, logError)
import Caide.Registry (findHtmlParser)
import Caide.Types


runHttpServer :: Verbosity -> F.FilePath -> IO ()
runHttpServer v root = do
    mbPorts <- runInDirectory v root getPorts
    case mbPorts of
        Left err -> logError $ T.pack $ describeError err
        Right (companionPort, chelperPort) -> runServers v root companionPort chelperPort


runServers :: Verbosity -> F.FilePath -> Int -> Int -> IO ()
runServers v root companionPort chelperPort = withSocketsDo $ do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    let servers = case (companionPort > 0, chelperPort > 0) of
            (True, True)   -> "CHelper and Competitive Companion extensions"
            (False, True)  -> "CHelper extension"
            (True, False)  -> "Competitive Companion extension"
            (False, False) -> ""
        runServer port handler = when (port > 0) $
            initServerBind port (tupleToHostAddress (127,0,0,1)) (handler v root)
    if T.null servers
    then logError "Both CHelper and Competitive Companion servers are disabled. Exiting now."
    else withAsync (void $ runInDirectory v root $ checkUpdates `catchError` const (pure ())) $ \a1 ->
         withAsync (void $ runServer companionPort processCompanionRequest) $ \a2 ->
         withAsync (void $ runServer chelperPort processCHelperRequest) $ \a3 -> do
             logInfo $ "Running HTTP server for " <> servers <> ". Press Return to terminate."
             _ <- getLine
             forM_ [a1, a2, a3] Async.cancel


getPorts :: CaideIO (Int, Int)
getPorts = do
    h <- readCaideConf
    companionPort <- getProp h "core" "companion_port" `orDefault` 10043
    chelperPort <- getProp h "core" "chelper_port" `orDefault` 4243
    return (companionPort, chelperPort)

data ParsedProblem = Parsed Problem [TestCase]

createProblems :: Verbosity -> F.FilePath -> [ParsedProblem] -> IO ()
createProblems _ _ [] = logError "The contest is empty"
createProblems v root parsedProblems = do
    ret <- runInDirectory v root $ do
        forM_ parsedProblems $ \(Parsed problem testCases) ->
            saveProblemWithScaffold problem testCases
        let Parsed problem _ = head parsedProblems
        setActiveProblem $ problemId problem

    case ret of
        Left err -> logError $ T.pack $ describeError err
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

processCompanionRequest :: Verbosity -> F.FilePath -> Request -> IO Response
processCompanionRequest v root request = do
    let body = LBS.fromStrict . encodeUtf8 . T.pack $ reqBody request
        mbParsed = eitherDecode' body :: Either String ParsedProblem
    when (v >= Debug) $
        putStrLn $ reqBody request
    case mbParsed of
        Left err -> do
            logError $ "Could not parse input JSON: " <> T.pack err
            return $ makeResponse badRequest err
        Right p -> do
            createProblems v root [p]
            return $ makeResponse ok "OK"


processCHelperRequest :: Verbosity -> F.FilePath -> Request -> IO Response
processCHelperRequest v root request = do
    let body = T.pack $ reqBody request
        bodyLines = T.lines body
        chid = T.strip $ head bodyLines
        page = T.unlines $ drop 1 bodyLines

    case () of
      _ | null bodyLines -> do
            logError "Invalid request!"
            return $ makeResponse badRequest "Invalid request!"
        | chid == "json"   -> return $ makeResponse ok "" -- Processed by Companion server instead
        | otherwise        -> do
            err <- process chid page v root
            case err of
                Nothing -> return $ makeResponse ok "OK"
                Just e  -> do
                    logError e
                    return $ makeResponse internalServerError $ T.unpack e


process :: T.Text -> T.Text -> Verbosity -> F.FilePath -> IO (Maybe T.Text)
process chid page v root = case parseFromHtml <$> findHtmlParser chid <*> Just page of
    Nothing -> return . Just $ T.concat $ ["'", chid, "' not supported"]
    Just (Left err) -> return . Just $ T.concat $ ["Error while parsing the problem: ", err]
    Just (Right (problem, testCases)) -> createProblems v root [Parsed problem testCases] >> return Nothing

