{-# LANGUAGE OverloadedStrings #-}
module Caide.Commands.CHelperHttpServer(
      runHttpServer
) where


import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (forkIO, killThread)
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Filesystem.Path as F
import Network
import System.IO (hClose)
import System.IO.Error (tryIOError)

import Data.Text.Encoding.Util (tryDecodeUtf8, universalNewlineConversionOnInput)
import qualified Data.Text.IO.Util as T

import Caide.Configuration (describeError, setActiveProblem)
import Caide.Commands.ParseProblem (saveProblem)
import Caide.Registry (findHtmlParser)
import Caide.Types


runHttpServer :: F.FilePath -> IO ()
runHttpServer root = withSocketsDo $ do
    sock <- listenOn $ PortNumber 4243
    putStrLn "Running HTTP server for CHelper extension. Press Return to terminate."
    tid <- forkIO $ void . sequence . repeat $ processRequest sock root
    _ <- getLine
    killThread tid
    sClose sock

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
            then putStrLn "Invalid request!"
            else process chid page root


process :: T.Text -> T.Text -> F.FilePath -> IO ()
process chid page root = case parseFromHtml <$> findHtmlParser chid <*> Just page of
    Nothing -> T.putStrLn . T.concat $ ["'", chid, "' not supported"]

    Just (Left err) -> T.putStrLn . T.concat $ ["Error while parsing the problem: ", err]

    Just (Right (problem, testCases)) -> do
        ret <- runInDirectory root $ do
            saveProblem problem testCases
            setActiveProblem $ problemId problem
        case ret of
            Left err -> putStrLn $ describeError err
            _        -> return ()

