{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Caide.Commands.Init(
      cmd
) where

import Control.Monad.State (liftIO)
import Codec.Archive.Zip (extractFilesFromArchive, toArchive, ZipOption(..))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict)
import Data.FileEmbed (embedFile)

import Filesystem.Path.CurrentOS (encodeString)
import qualified Filesystem.Path as FSP

import System.Environment (getExecutablePath)

import Caide.Configuration (writeCaideConf, writeCaideState, defaultCaideConf, defaultCaideState)
import Caide.Types

cmd :: CommandHandler
cmd = CommandHandler
    { command = "init"
    , description = "Initialize current caide directory"
    , usage = ""
    , action = initialize
    }

initialize :: [String] -> CaideIO ()
initialize _ = do
    curDir <- caideRoot
    caideExe <- liftIO getExecutablePath
    _ <- writeCaideConf $ defaultCaideConf curDir
    _ <- writeCaideState $ defaultCaideState caideExe
    liftIO $ do
        unpackResources curDir
        putStrLn $ "Initialized caide directory at " ++ encodeString curDir


-- This zip file is prepared in advance in Setup.hs
resourcesZipFile :: BS.ByteString
resourcesZipFile = $(embedFile "res/init.zip")

unpackResources :: FSP.FilePath -> IO ()
unpackResources rootDir = do
    let archive = toArchive $ fromStrict resourcesZipFile
        destination = encodeString rootDir
        options = [OptDestination destination]
    extractFilesFromArchive options archive

