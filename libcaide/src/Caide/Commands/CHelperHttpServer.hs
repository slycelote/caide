{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Caide.Commands.CHelperHttpServer(
      runHttpServer
) where

#ifndef AMP
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, killThread)
import Control.Monad (forM_, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Filesystem.Path as F
import qualified Filesystem.Path.CurrentOS as F
import Network (accept, listenOn, sClose, withSocketsDo, PortID(PortNumber), Socket)
import Network.Socket (tupleToHostAddress)
import System.IO (hClose, hSetBuffering, stdout, stderr, BufferMode(NoBuffering))
import System.IO.Error (tryIOError)

import Data.Aeson (FromJSON(parseJSON), Object, eitherDecode', withObject, (.:))
import Data.Aeson.Types (Parser)
import Network.Shed.Httpd (initServerBind, Request(reqBody), Response(Response))

import Data.Text.Encoding.Util (tryDecodeUtf8, universalNewlineConversionOnInput)
import qualified Data.Text.IO.Util as T

import Caide.Configuration (describeError, orDefault, readCaideConf, setActiveProblem)
import Caide.Commands.ParseProblem (saveProblem)
import Caide.Registry (findHtmlParser)
import Caide.Types


runHttpServer :: F.FilePath -> IO ()
runHttpServer root = do
    mbPorts <- runInDirectory root getPorts
    case mbPorts of
        Left err -> putStrLn $ describeError err
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
        else do
            sock <- listenOn $ PortNumber $ fromIntegral chelperPort
            chelperThreadId <- forkIO $ void . sequence . repeat $ processRequest sock root
            return $ Just (sock, chelperThreadId)

    let servers = case (mbCompanion, mbChelper) of
            (Just _, Just _)   -> "CHelper and Competitive Companion extensions"
            (Nothing, Just _)  -> "CHelper extension"
            (Just _, Nothing)  -> "Competitive Companion extension"
            (Nothing, Nothing) -> ""

    if T.null servers
    then T.putStrLn "Both CHelper and Competitive Companion servers are disabled. Exiting now."
    else do
        T.putStrLn $ T.concat ["Running HTTP server for ", servers, ". Press Return to terminate."]
        _ <- getLine
        case mbChelper of
            Just (sock, chelperThreadId) -> killThread chelperThreadId >> sClose sock
            Nothing                      -> return ()
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
createProblems _ [] = putStrLn "The contest is empty"
createProblems root parsedProblems = do
    ret <- runInDirectory root $ do
        forM_ parsedProblems $ \(Parsed problem testCases) ->
            saveProblem problem testCases
        let Parsed problem _ = head parsedProblems
        setActiveProblem $ problemId problem

    case ret of
        Left err -> putStrLn $ describeError err
        _        -> return ()


ok, badRequest :: Int
ok = 200
badRequest = 400

makeResponse :: Int -> String -> Response
makeResponse code message = Response code [] message


instance FromJSON InputSource where
  parseJSON = withObject "input description" $ \o -> do
    inputType <- o .: "type"
    if inputType == ("file" :: String) then FileInput . F.fromText <$> o .: "fileName" else pure StdIn

instance FromJSON OutputTarget where
  parseJSON = withObject "output description" $ \o -> do
    outputType <- o .: "type"
    if outputType == ("file" :: String) then FileOutput . F.fromText <$> o .: "fileName" else pure StdOut

instance FromJSON TestCase where
  parseJSON = withObject "test case" $ \o ->
    TestCase <$> o .: "input" <*> o .: "output"

parseProblemFromObject :: Object -> Parser ParsedProblem
parseProblemFromObject o = do
    input <- o .: "input"
    output <- o .: "output"
    probName <- o .: "name"
    tests <- o .: "tests"
    languages <- o .: "languages"
    java <- languages .: "java"
    probId <- java .: "taskClass"
    return $ Parsed (Problem probName probId (Stream input output)) tests

instance FromJSON ParsedProblem where
  parseJSON = withObject "problem description" parseProblemFromObject

newtype Contest = Contest [ParsedProblem]
instance FromJSON Contest where
  parseJSON = withObject "a problem or a contest" $ \o -> oneProblem o <|> contest o
    where
      oneProblem o = do problem <- parseProblemFromObject o
                        return $ Contest [problem]
      contest o = do result <- o .: "result"
                     Contest <$> parseJSON result


processCompanionRequest :: F.FilePath -> Request -> IO Response
processCompanionRequest root request = do
    let body = LBS.fromStrict . encodeUtf8 . T.pack $ reqBody request
        mbParsed = eitherDecode' body :: Either String Contest
    case mbParsed of
        Left err -> do
            putStrLn $ "Could not parse input JSON: " ++ err
            return $ makeResponse badRequest err
        Right (Contest problems) -> do
            createProblems root problems
            return $ makeResponse ok "OK"


processRequest :: Socket -> F.FilePath -> IO ()
processRequest sock root = void . tryIOError $ do
    (h, _, _) <- accept sock
    decoded <- tryDecodeUtf8 <$> BS.hGetContents h
    case decoded of
        Left err -> T.putStrLn . T.concat $ ["Invalid request: ", err]
        Right cont -> do
            let body = drop 1 . dropWhile (not . T.null . T.strip) .
                       T.lines . universalNewlineConversionOnInput $ cont
                chid = T.strip $ head body
                page = T.unlines $ drop 1 body

            T.length page `seq` hClose h

            case () of
              _ | null body      -> T.putStrLn "Invalid request!"
                | chid == "json" -> return () -- Processed by Companion server instead
                | otherwise      -> process chid page root


process :: T.Text -> T.Text -> F.FilePath -> IO ()
process chid page root = case parseFromHtml <$> findHtmlParser chid <*> Just page of
    Nothing -> T.putStrLn . T.concat $ ["'", chid, "' not supported"]
    Just (Left err) -> T.putStrLn . T.concat $ ["Error while parsing the problem: ", err]
    Just (Right (problem, testCases)) -> createProblems root [Parsed problem testCases]

