module Caide.CPP.CPP (
      language
) where

import Control.Applicative ((<$>))
import Data.List (partition)
import qualified Data.Text as T

import Prelude hiding (FilePath)
import Filesystem (isFile, copyFile, readTextFile, writeTextFile, listDirectory, isDirectory)
import Filesystem.Path.CurrentOS (decodeString)
import Filesystem.Path ((</>), FilePath, hasExtension)

import Text.Regex.TDFA.Text (Regex, compile, execute)
import Text.Regex.Base.RegexLike (defaultExecOpt, defaultCompOpt)


import qualified Caide.CPP.CPPSimple as CPPSimple

import Caide.CPP.CBinding
import Caide.Types
import Caide.Util (getProblemID, splitString)


language :: ProgrammingLanguage
language = CPPSimple.language {inlineCode = inlineCPPCode}

inlineCPPCode :: CaideEnvironment -> FilePath -> IO ()
inlineCPPCode env problemDir = do
    let probID = getProblemID problemDir
        caideRoot = getRootDirectory env
        solutionPath = problemDir </> decodeString (probID ++ ".cpp")
        inlinedTemplatePath =  caideRoot </> decodeString "templates" </> decodeString "main_template.cpp"
        inlinedCodePath = problemDir </> decodeString ".caideproblem" </> decodeString "inlined.cpp"
        inlinedNoPragmaOnceCodePath = problemDir </> decodeString ".caideproblem" </> decodeString "inlinedNoPragmaOnce.cpp"
        finalCodePath = problemDir </> decodeString "submission.cpp"
        libraryDirectory = caideRoot </> decodeString "cpplib"

    libExists <- isDirectory libraryDirectory
    libraryCPPFiles <- if libExists
                       then filter (`hasExtension` T.pack "cpp") <$> listDirectoryRecursively libraryDirectory
                       else return []

    systemHeaderDirs <- map decodeString . splitString "\r\n," <$> getUserOption env "cpp" "system_header_dirs"

    inlineLibraryCode (solutionPath:inlinedTemplatePath:libraryCPPFiles) systemHeaderDirs [libraryDirectory] inlinedCodePath
    removePragmaOnceFromFile inlinedCodePath inlinedNoPragmaOnceCodePath
    removeUnusedCode inlinedNoPragmaOnceCodePath systemHeaderDirs finalCodePath
    copyFile finalCodePath $ caideRoot </> decodeString "submission.cpp"


listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively dir = do
    filesAndDirs <- listDirectory dir
    thisIsFile <- mapM isFile filesAndDirs
    let (files', dirs') = partition snd $ zip filesAndDirs thisIsFile
        (files, dirs) = (map fst files', map fst dirs')
    recList <- concat <$> mapM listDirectoryRecursively dirs
    return $ files ++ recList

removePragmaOnceFromFile :: FilePath -> FilePath -> IO ()
removePragmaOnceFromFile inputPath outputPath =
    readTextFile inputPath >>= writeTextFile outputPath . removePragmaOnce

pragmaOnceRegex :: Regex
Right pragmaOnceRegex = compile defaultCompOpt defaultExecOpt . T.pack $ "^" ++ space ++ "*#" ++ space ++ "*pragma" ++ space ++ "+once" ++ space ++ "*$"
    where space = "[ \\t\\r\\n\\v\\f]"

removePragmaOnce :: T.Text -> T.Text
removePragmaOnce = T.unlines . filter (not . isPragmaOnce) . T.lines
    where isPragmaOnce = isMatch . execute pragmaOnceRegex
          isMatch (Left _) = False
          isMatch (Right Nothing) = False
          isMatch _ = True

