{-# LANGUAGE CPP, TemplateHaskell, OverloadedStrings #-}

module Caide.Commands.Init(
      initialize
) where

import Control.Monad.State (liftIO)
import Codec.Archive.Zip (extractFilesFromArchive, toArchive, ZipOption(..))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict)
import Data.FileEmbed (embedFile)
import qualified Data.Text as T
import qualified Data.Text.IO.Util as T

import Filesystem.Path.CurrentOS (encodeString)
import qualified Filesystem.Path as FSP
import Filesystem.Util (pathToText)

import Caide.Configuration (writeCaideConf, writeCaideState, defaultCaideConf, defaultCaideState)
import Caide.Types

initialize :: Bool -> CaideIO ()
initialize useSystemCppHeaders = do
    curDir <- caideRoot
    _ <- writeCaideConf $ defaultCaideConf curDir useSystemCppHeaders
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

