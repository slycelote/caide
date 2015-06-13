{-# LANGUAGE CPP, TemplateHaskell, OverloadedStrings #-}

module Caide.Commands.Init(
      initialize
) where

import Control.Monad (forM_)
import Control.Monad.State (liftIO)
import Codec.Archive.Zip (extractFilesFromArchive, toArchive, ZipOption(..))
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict)
import Data.FileEmbed (embedFile)
import qualified Data.Text as T
import qualified Data.Text.IO.Util as T

import Filesystem (createTree)
import Filesystem.Path.CurrentOS (encodeString, (</>))
import qualified Filesystem.Path as FSP
import Filesystem.Util (pathToText, writeTextFile)

import Caide.Configuration (writeCaideConf, writeCaideState, defaultCaideConf, defaultCaideState)
import Caide.Templates (templates)
import Caide.Types

initialize :: Bool -> CaideIO ()
initialize useSystemCppHeaders = do
    curDir <- caideRoot
    _ <- writeCaideConf $ defaultCaideConf curDir useSystemCppHeaders
    _ <- writeCaideState defaultCaideState
    liftIO $ do
        unpackResources curDir
        createTree $ curDir </> "templates"
        createTree $ curDir </> ".caide" </> "templates"
        forM_ templates $ \(fileName, cont) -> do
            writeTextFile (curDir </> "templates" </> fileName) cont
            writeTextFile (curDir </> ".caide" </> "templates" </> fileName) cont
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

