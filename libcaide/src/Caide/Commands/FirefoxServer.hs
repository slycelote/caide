{-# LANGUAGE CPP, OverloadedStrings #-}
module Caide.Commands.FirefoxServer(
      runFirefoxServer
) where

import qualified Data.ByteString.Lazy as BS
import Data.HashMap.Lazy ((!))
import qualified Data.HashMap.Lazy as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.IO (hFlush, hPutStrLn, hSetBuffering, stderr, stdout, BufferMode(NoBuffering))


import Data.Aeson (Value(Object, Array, String), eitherDecode')
import qualified Data.Aeson as Json
import Data.Binary (Binary(get, put), decodeOrFail, encode)
import Data.Binary.Get (getWord32host, getLazyByteString)
import Data.Binary.Put (putWord32host, putLazyByteString)
import Filesystem.Path.CurrentOS (fromText)


import Caide.Commands.ParseProblem (saveProblem)
import Caide.Configuration (describeError)
import Caide.Registry (findHtmlParserForUrl)
import Caide.Types


processMessage :: Value -> IO Value
processMessage (Object kvMap) = do
    ret <- runInDirectory (fromText caideDir) $ case () of
        () | probType `elem` ["default", "stream"] -> processDefault url doc
           | probType == "dcj" -> processDcj probTitle (map getString $ V.toList sampleInputs)
        _ -> throw "Unexpected problem type"
    return $ case ret of
        Left err -> errorMessage . T.pack . describeError $ err
        _        -> successMessage

  where
    String probType = kvMap!"problemType"
    String probTitle = kvMap!"problemTitle"
    String caideDir = kvMap!"caideDir"
    String url = kvMap!"url"
    String doc = kvMap!"document"
    Array sampleInputs = kvMap!"sampleInputs"
    getString (String s) = s
    getString _ = error "Unexpected JSON input"

processMessage _ = error "Unexpected JSON input"


successMessage :: Value
successMessage = Object $ Map.empty

errorMessage :: Text -> Value
errorMessage err = Object $ Map.singleton "error" (String err)


processDefault :: URL -> Text -> CaideIO ()
processDefault url doc = do
    let Just htmlParser = findHtmlParserForUrl url
        parseResult = parseFromHtml htmlParser doc
    case parseResult of
        Left err -> throw . T.unlines $ ["Encountered a problem while parsing:", err]
        Right (problem, samples) -> saveProblem problem samples


processDcj :: Text -> [Text] -> CaideIO ()
processDcj probTitle sampleInputHeaders = saveProblem problem samples
  where
    problem = Problem
            { problemName = probTitle
            , problemId = probTitle
            , problemType = DCJ
            }
    samples = [TestCase input "" | input <- sampleInputHeaders]

-- In Firefox native messaging, a JSON message is preceded by its byte length in host order.
newtype Message = Message Value

instance Binary Message where
    put (Message value) = putWord32host (fromIntegral $ BS.length ser) >> putLazyByteString ser
      where ser = Json.encode value

    get = do
        len <- getWord32host
        ser <- getLazyByteString $ fromIntegral len
        case eitherDecode' ser of
            Right value -> return $ Message value
            Left e      -> fail e


runFirefoxServer :: IO ()
runFirefoxServer = do
    hSetBuffering stdout NoBuffering
    input <- BS.getContents
    processLoop input
  where
    processLoop s | BS.null s = return ()
    processLoop s = case decodeOrFail s of
        Left (_, _, err) -> hPutStrLn stderr err
        Right (rest, _, Message value) -> do
            outputValue <- processMessage value
            BS.putStr . encode $ Message outputValue
            hFlush stdout
            processLoop rest

