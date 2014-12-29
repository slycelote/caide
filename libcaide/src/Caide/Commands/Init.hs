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
    prepareCppLibrary curDir
    putStrLn $ "Initialized caide directory at " ++ encodeString curDir
    return Nothing

defaultTemplates :: [(FilePath, String)]
defaultTemplates = [
#include "defaultTemplates.inc"
    ]

prepareCppLibrary :: FSP.FilePath -> IO ()

#ifdef CLANG_INLINER
resourcesZipFile :: Data.ByteString.ByteString
resourcesZipFile = $(embedFile "res/headers.zip")

prepareCppLibrary rootDir = do
    let archive = toArchive $ fromStrict resourcesZipFile
        destination = encodeString $ rootDir </> decodeString "include"
        options = [OptDestination destination]
        cpplibDir = rootDir </> decodeString "cpplib"
    extractFilesFromArchive options archive
    createDirectory True cpplibDir
    writeTextFile (cpplibDir </> decodeString "README.txt") $
        T.pack "Put *.h and *.cpp files of your library in this directory."
    writeTextFile (cpplibDir </> decodeString "dummy.cpp") $
        T.pack "//force creation of static library\nclass Dummy_Caide_Class_ {};\n"
#else
prepareCppLibrary _ = return ()
#endif

