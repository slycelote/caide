module Caide.Commands.Init(
      cmd
) where

import Filesystem (createDirectory)

import qualified Filesystem.Path as F
import Filesystem.Path.CurrentOS (decodeString, encodeString, (</>))

import Caide.Types

cmd :: CommandHandler
cmd = CommandHandler
    { command = "init"
    , description = "Initialize current caide directory"
    , usage = ""
    , action = initialize
    }

initialize :: F.FilePath -> [String] -> IO ()
initialize curDir _ = do
    let caideDir = curDir </> decodeString ".caide"
    createDirectory False caideDir
    putStrLn $ "Initialized caide directory at " ++ encodeString curDir
