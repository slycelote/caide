{-# LANGUAGE CPP #-}
#ifdef CLANG_INLINER
{-# LANGUAGE TemplateHaskell #-}
#endif

module Caide.Commands.Init(
      cmd
) where

import Codec.Archive.Zip (extractFilesFromArchive, toArchive, ZipOption(..))
import Control.Monad (forM_)
import qualified Data.ByteString
import Data.ByteString.Lazy (fromStrict)
import Data.FileEmbed (embedFile)
import qualified Data.Text as T
import Filesystem (writeTextFile, createDirectory)

import Filesystem.Path.CurrentOS (decodeString, encodeString, (</>))
import qualified Filesystem.Path as FSP

import Caide.Types

cmd :: CommandHandler
cmd = CommandHandler
    { command = "init"
    , description = "Initialize current caide directory"
    , usage = ""
    , action = initialize
    }

initialize :: CaideEnvironment -> [String] -> IO (Maybe String)
initialize env _ = do
    let curDir = getRootDirectory env
        templatesDir = curDir </> decodeString "templates"
    createDirectory True templatesDir
    forM_ defaultTemplates $ \(fileName, fileContents) ->
        writeTextFile (templatesDir </> decodeString fileName) (T.pack fileContents)
    unzipHeaders curDir
    putStrLn $ "Initialized caide directory at " ++ encodeString curDir
    return Nothing

defaultTemplates :: [(FilePath, String)]
defaultTemplates = [
#include "defaultTemplates.inc"
    ]

unzipHeaders :: FSP.FilePath -> IO ()

#ifdef CLANG_INLINER
resourcesZipFile :: Data.ByteString.ByteString
resourcesZipFile = $(embedFile "res/headers.zip")

unzipHeaders rootDir = extractFilesFromArchive options archive
    where archive = toArchive $ fromStrict resourcesZipFile
          destination = encodeString $ rootDir </> decodeString "include"
          options = [OptDestination destination]
#else
unzipHeaders _ = return ()
#endif

