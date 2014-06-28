{-# LANGUAGE CPP #-}

module Caide.Commands.Init(
      cmd
) where

import Control.Monad (forM_)
import qualified Data.Text as T
import Filesystem (writeTextFile, createDirectory)

import Filesystem.Path.CurrentOS (decodeString, encodeString, (</>))

import Caide.Types

cmd :: CommandHandler
cmd = CommandHandler
    { command = "init"
    , description = "Initialize current caide directory"
    , usage = ""
    , action = initialize
    }

initialize :: CaideEnvironment -> [String] -> IO ()
initialize env _ = do
    let curDir = getRootDirectory env
        templatesDir = curDir </> decodeString "templates"
    createDirectory True templatesDir
    forM_ defaultTemplates $ \(fileName, fileContents) ->
        writeTextFile (templatesDir </> decodeString fileName) (T.pack fileContents)
    putStrLn $ "Initialized caide directory at " ++ encodeString curDir

defaultTemplates :: [(FilePath, String)]
defaultTemplates = [
#include "defaultTemplates.inc"
    ]
