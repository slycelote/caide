{-# LANGUAGE CPP, TemplateHaskell, OverloadedStrings #-}

module Caide.Commands.Init(
      cmd
) where

import Control.Monad.State (liftIO)
import Codec.Archive.Zip (extractFilesFromArchive, toArchive, ZipOption(..))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict)
import Data.FileEmbed (embedFile)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Filesystem.Path.CurrentOS (encodeString)
import qualified Filesystem.Path as FSP

import Caide.Configuration (writeCaideConf, writeCaideState, defaultCaideConf, defaultCaideState)
import Caide.Types
import Caide.Util (pathToText)

cmd :: CommandHandler
cmd = CommandHandler
    { command = "init"
    , description = "Initialize current caide directory"
    , usage = "caide init [--cpp-use-system-headers]"
    , action = initialize
    }

initialize :: [T.Text] -> CaideIO ()
initialize args = do
    let useSystemHeaders = "--cpp-use-system-headers" `elem` args
    curDir <- caideRoot
    _ <- writeCaideConf $ defaultCaideConf curDir useSystemHeaders
    _ <- writeCaideState defaultCaideState
    liftIO $ do
        unpackResources curDir
        T.putStrLn . T.concat $ ["Initialized caide directory at ", pathToText curDir]


-- This zip file is prepared in advance in Setup.hs
resourcesZipFile :: BS.ByteString
resourcesZipFile = $(embedFile "res/init.zip")

unpackResources :: FSP.FilePath -> IO ()
unpackResources rootDir = do
    let archive = toArchive $ fromStrict resourcesZipFile
        destination = encodeString rootDir
        options = [OptDestination destination]
    extractFilesFromArchive options archive

