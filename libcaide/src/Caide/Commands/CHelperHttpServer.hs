{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Caide.Commands.CHelperHttpServer(
      runHttpServer
) where

#ifndef AMP
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, killThread)
import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Filesystem.Path as F
import qualified Filesystem.Path.CurrentOS as F
import Network.Socket (tupleToHostAddress, withSocketsDo)
import System.IO (hSetBuffering, stdout, stderr, BufferMode(NoBuffering))

import Data.Aeson (FromJSON(parseJSON), eitherDecode', withObject, (.:))
import Network.Shed.Httpd (initServerBind, Request(reqBody), Response(Response))

import Caide.Configuration (describeError, orDefault, readCaideConf, setActiveProblem)
import Caide.Commands.ParseProblem (saveProblemWithScaffold)
import Caide.Logger (logInfo, logError)
import Caide.Registry (findHtmlParser)
import Caide.Types


runHttpServer :: F.FilePath -> IO ()
runHttpServer root = do
    mbPorts <- runInDirectory root getPorts
    case mbPorts of
        Left err -> logError $ T.pack $ describeError err
        Right (companionPort, chelperPort) -> runServers root companionPort chelperPort


runServers :: F.FilePath -> Int -> Int -> IO ()
runServers root companionPort chelperPort = withSocketsDo $ do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    mbCompanion <- if companionPort <= 0
        then return Nothing
        else Just <$> (forkIO $ initServerBind companionPort (tupleToHostAddress (127,0,0,1)) (processCompanionRequest root))
    mbChelper <- if chelperPort <= 0
        then return Nothing
        else Just <$> (forkIO $ initServerBind chelperPort (tupleToHostAddress (127,0,0,1)) (processCHelperRequest root))

    let servers = case (mbCompanion, mbChelper) of
            (Just _, Just _)   -> "CHelper and Competitive Companion extensions"
            (Nothing, Just _)  -> "CHelper extension"
            (Just _, Nothing)  -> "Competitive Companion extension"
            (Nothing, Nothing) -> ""

    if T.null servers
    then logError "Both CHelper and Competitive Companion servers are disabled. Exiting now."
    else do
        logInfo $ "Running HTTP server for " <> servers <> ". Press Return to terminate."
        _ <- getLine
        case mbChelper of
            Just chelperThreadId -> killThread chelperThreadId
            Nothing              -> return ()
        case mbCompanion of
            Just companionThreadId -> killThread companionThreadId
            Nothing                -> return ()


getPorts :: CaideIO (Int, Int)
getPorts = do
    h <- readCaideConf
    companionPort <- getProp h "core" "companion_port" `orDefault` 10043
    chelperPort <- getProp h "core" "chelper_port" `orDefault` 4243
    return (companionPort, chelperPort)

data ParsedProblem = Parsed Problem [TestCase]

createProblems :: F.FilePath -> [ParsedProblem] -> IO ()
createProblems _ [] = logError "The contest is empty"
createProblems root parsedProblems = do
    ret <- runInDirectory root $ do
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

processCompanionRequest :: F.FilePath -> Request -> IO Response
processCompanionRequest root request = do
    let body = LBS.fromStrict . encodeUtf8 . T.pack $ reqBody request
        mbParsed = eitherDecode' body :: Either String ParsedProblem
    case mbParsed of
        Left err -> do
            logError $ "Could not parse input JSON: " <> T.pack err
            return $ makeResponse badRequest err
        Right p -> do
            createProblems root [p]
            return $ makeResponse ok "OK"


processCHelperRequest :: F.FilePath -> Request -> IO Response
processCHelperRequest root request = do
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
            err <- process chid page root
            case err of
                Nothing -> return $ makeResponse ok "OK"
                Just e  -> do
                    logError e
                    return $ makeResponse internalServerError $ T.unpack e


process :: T.Text -> T.Text -> F.FilePath -> IO (Maybe T.Text)
process chid page root = case parseFromHtml <$> findHtmlParser chid <*> Just page of
    Nothing -> return . Just $ T.concat $ ["'", chid, "' not supported"]
    Just (Left err) -> return . Just $ T.concat $ ["Error while parsing the problem: ", err]
    Just (Right (problem, testCases)) -> createProblems root [Parsed problem testCases] >> return Nothing

