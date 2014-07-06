module Caide.CPP.CPP (
      language
) where

import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (decodeString)
import Filesystem.Path ((</>), FilePath, hasExtension)

import qualified Data.Text as T

import qualified Caide.CPP.CPPSimple as CPPSimple

import Caide.CPP.CBinding
import Caide.Types
import Caide.Util (getProblemID, splitString)
import Filesystem (isFile, listDirectory, isDirectory)
import Data.List (partition)
import Control.Applicative ((<$>))

language :: ProgrammingLanguage
language = CPPSimple.language {inlineCode = inlineCPPCode}

inlineCPPCode :: CaideEnvironment -> FilePath -> IO ()
inlineCPPCode env problemDir = do
    let probID = getProblemID problemDir
        caideRoot = getRootDirectory env
        solutionPath = problemDir </> decodeString (probID ++ ".cpp")
        inlinedTemplatePath =  caideRoot </> decodeString "templates" </> decodeString "main_template.cpp"
        inlinedCodePath = problemDir </> decodeString ".caideproblem" </> decodeString "inlined.cpp"
        finalCodePath = problemDir </> decodeString "main.cpp"
        libraryDirectory = caideRoot </> decodeString "cpplib"

    libExists <- isDirectory libraryDirectory
    libraryCPPFiles <- if libExists
                           then filter (`hasExtension` T.pack "cpp") <$> listDirectoryRecursively libraryDirectory
                           else return []

    systemHeaderDirs <- map decodeString . splitString "\r\n," <$> getUserOption env "cpp" "system_header_dirs"

    inlineLibraryCode (solutionPath:inlinedTemplatePath:libraryCPPFiles) systemHeaderDirs [libraryDirectory] inlinedCodePath
    removeUnusedCode inlinedCodePath systemHeaderDirs finalCodePath

listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively dir = do
    filesAndDirs <- listDirectory dir
    thisIsFile <- mapM isFile filesAndDirs
    let (files', dirs') = partition snd $ zip filesAndDirs thisIsFile
        (files, dirs) = (map fst files', map fst dirs')
    recList <- concat <$> mapM listDirectoryRecursively dirs
    return $ files ++ recList
