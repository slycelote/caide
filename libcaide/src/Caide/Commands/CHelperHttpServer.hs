{-# LANGUAGE CPP, OverloadedStrings #-}
module Caide.Commands.CHelperHttpServer(
      runHttpServer
) where

#ifndef AMP
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Concurrent (forkIO, killThread)
import Control.Monad (void)
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

import Data.Aeson (FromJSON(parseJSON), eitherDecode', withObject, (.:))
import Network.Shed.Httpd (initServerBind, Request(..), Response(..))

import Data.Text.Encoding.Util (tryDecodeUtf8, universalNewlineConversionOnInput)
import qualified Data.Text.IO.Util as T

import Caide.Configuration (describeError, setActiveProblem)
import Caide.Commands.ParseProblem (saveProblem)
import Caide.Registry (findHtmlParser)
import Caide.Types


runHttpServer :: F.FilePath -> IO ()
runHttpServer root = withSocketsDo $ do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    sock <- listenOn $ PortNumber 4243
    T.putStrLn "Running HTTP server for CHelper and Competitive Companion extensions. Press Return to terminate."
    chelperThreadId <- forkIO $ void . sequence . repeat $ processRequest sock root
    companionThreadId <- forkIO $ initServerBind 8080 (tupleToHostAddress (127,0,0,1)) (processCompanionRequest root)
    _ <- getLine
    killThread companionThreadId
    killThread chelperThreadId
    sClose sock

data ParsedProblem = Parsed Problem [TestCase]

createProblem :: F.FilePath -> ParsedProblem -> IO ()
createProblem root (Parsed problem testCases) = do
   ret <- runInDirectory root $ do
       saveProblem problem testCases
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
  parseJSON = withObject "input" $ \o -> do
    inputType <- o .: "type"
    if inputType == ("file" :: String) then FileInput . F.fromText <$> o .: "fileName" else pure StdIn

instance FromJSON OutputTarget where
  parseJSON = withObject "output" $ \o -> do
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
    return $ Parsed (Problem probName probId (Stream input output)) tests

processCompanionRequest :: F.FilePath -> Request -> IO Response
processCompanionRequest root request = do
    let body = LBS.fromStrict . encodeUtf8 . T.pack $ reqBody request
        mbParsed = eitherDecode' body
    case mbParsed of
        Left err -> do
            putStrLn err
            return $ makeResponse badRequest err
        Right parsedProblem -> do
            createProblem root parsedProblem
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

            if null body
            then T.putStrLn "Invalid request!"
            else process chid page root


process :: T.Text -> T.Text -> F.FilePath -> IO ()
process chid page root = case parseFromHtml <$> findHtmlParser chid <*> Just page of
    Nothing -> T.putStrLn . T.concat $ ["'", chid, "' not supported"]
    Just (Left err) -> T.putStrLn . T.concat $ ["Error while parsing the problem: ", err]
    Just (Right (problem, testCases)) -> createProblem root $ Parsed problem testCases

