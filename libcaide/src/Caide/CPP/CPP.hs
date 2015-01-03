module Caide.CPP.CPP (
      language
) where

import Control.Applicative ((<$>))
import Control.Monad.State (liftIO)
import qualified Data.Text as T

import Prelude hiding (FilePath)
import Filesystem (copyFile, readTextFile, writeTextFile, isDirectory)
import Filesystem.Path.CurrentOS (decodeString)
import Filesystem.Path ((</>), FilePath, hasExtension)

import Text.Regex.TDFA.Text (Regex)
import Text.Regex.Base.RegexLike (makeRegex, match)


import qualified Caide.CPP.CPPSimple as CPPSimple

import Caide.Configuration (getListProp, readCaideConf)
import Caide.CPP.CBinding
import Caide.Types
import Caide.Util (getProblemID, listDir)


language :: ProgrammingLanguage
language = CPPSimple.language {inlineCode = inlineCPPCode}

inlineCPPCode :: FilePath -> CaideIO ()
inlineCPPCode problemDir = do
    root <- caideRoot
    let probID = getProblemID problemDir
        solutionPath = problemDir </> decodeString (probID ++ ".cpp")
        inlinedTemplatePath =  root </> decodeString "templates" </> decodeString "main_template.cpp"
        inlinedCodePath = problemDir </> decodeString ".caideproblem" </> decodeString "inlined.cpp"
        inlinedNoPragmaOnceCodePath = problemDir </> decodeString ".caideproblem" </> decodeString "inlinedNoPragmaOnce.cpp"
        finalCodePath = problemDir </> decodeString "submission.cpp"
        libraryDirectory = root </> decodeString "cpplib"

    hConf <- readCaideConf
    systemHeaderDirs <- map decodeString <$> getListProp hConf "cpp" "system_header_dirs"

    liftIO $ do
        libExists <- isDirectory libraryDirectory
        libraryCPPFiles <- if libExists
                           then filter (`hasExtension` T.pack "cpp") <$> listDirectoryRecursively libraryDirectory
                           else return []

        inlineLibraryCode (solutionPath:inlinedTemplatePath:libraryCPPFiles) systemHeaderDirs [libraryDirectory] inlinedCodePath
        removePragmaOnceFromFile inlinedCodePath inlinedNoPragmaOnceCodePath
        removeUnusedCode inlinedNoPragmaOnceCodePath systemHeaderDirs finalCodePath
        copyFile finalCodePath $ root </> decodeString "submission.cpp"


listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively dir = do
    (files, dirs) <- listDir dir
    recList <- concat <$> mapM listDirectoryRecursively dirs
    return $ files ++ recList

removePragmaOnceFromFile :: FilePath -> FilePath -> IO ()
removePragmaOnceFromFile inputPath outputPath =
    readTextFile inputPath >>= writeTextFile outputPath . removePragmaOnce

pragmaOnceRegex :: Regex
pragmaOnceRegex = makeRegex . T.pack $ "^[[:space:]]*#[[:space:]]*pragma[[:space:]]+once[[:space:]]*$"

removePragmaOnce :: T.Text -> T.Text
removePragmaOnce = T.unlines . filter (not . isPragmaOnce) . T.lines
    where isPragmaOnce = match pragmaOnceRegex

