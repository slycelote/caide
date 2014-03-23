module Caide.Commands.Init(
      cmd
) where

import Control.Monad (forM_)
import qualified Data.Text as T
import Filesystem (createDirectory, writeTextFile)

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
    forM_ defaultTemplates $ \(fileName, fileContents) ->
        writeTextFile (curDir </> decodeString fileName) (T.pack fileContents)
    putStrLn $ "Initialized caide directory at " ++ encodeString curDir

-- TODO: hardcode text of templates here when finalized
defaultTemplates :: [(FilePath, String)]
defaultTemplates = [
    ("solution_template.cpp",

    "#include <iostream>\n\
    \\n\
    \void solve(std::istream& in, std::ostream& out)\n\
    \{\n\
    \}\n\
    \"),

    ("main_template.cpp",
    ""),

    ("test_template.cpp",
    "")

    ]
