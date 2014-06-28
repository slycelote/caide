module Caide.Commands.Init(
      cmd
) where

import Control.Monad (forM_)
import qualified Data.Text as T
import Filesystem (writeTextFile)

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
    forM_ defaultTemplates $ \(fileName, fileContents) ->
        writeTextFile (curDir </> decodeString fileName) (T.pack fileContents)
    putStrLn $ "Initialized caide directory at " ++ encodeString curDir

-- TODO: hardcode text of templates here when finalized
defaultTemplates :: [(FilePath, String)]
defaultTemplates = [
    ("solution_template.cpp",

    ""),

    ("main_template.cpp",
    ""),

    ("test_template.cpp",
    "")

    ]
