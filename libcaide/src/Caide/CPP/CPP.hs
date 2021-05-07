{-# LANGUAGE CPP, OverloadedStrings #-}
module Caide.CPP.CPP(
      language
) where

import Control.Monad (when)
import Control.Monad.State (liftIO)
import qualified Data.Text as T

import Prelude hiding (FilePath)
import Filesystem (copyFile, isDirectory)
import Filesystem.Path.CurrentOS (fromText)
import Filesystem.Path ((</>), FilePath, hasExtension)

import Filesystem.Util (listDir)

import qualified Caide.CPP.CPPSimple as CPPSimple

import Caide.Configuration (readCaideConf, withDefault)
import Caide.CPP.CBinding (inlineLibraryCode)
import qualified Caide.Problem as Problem
import Caide.Types
import Caide.Util (tshow)


language :: ProgrammingLanguage
language = CPPSimple.language {inlineCode = inlineCPPCode}


inlineCPPCode :: ProblemID -> CaideIO ()
inlineCPPCode probID = do
    root <- caideRoot
    let problemDir = root </> fromText probID
        solutionPath = problemDir </> fromText (T.append probID ".cpp")
        mainFilePath = problemDir </> "main.cpp"
        tempDir = problemDir </> ".caideproblem"
        libraryDirectory = root </> "cpplib"

    hConf <- readCaideConf
    cmdLineOptions <- getProp hConf "cpp" "clang_options"
    macrosToKeep <- withDefault ["ONLINE_JUDGE"] $ getProp hConf "cpp" "keep_macros"
    maxConsequentEmptyLines <- withDefault 2 $ getProp hConf "cpp" "max_consequent_empty_lines"

    problem <- Problem.readProblemInfo probID

    libExists <- liftIO $ isDirectory libraryDirectory
    libraryCPPFiles <- if libExists
                       then filter (`hasExtension` "cpp") <$> liftIO (listDirectoryRecursively libraryDirectory)
                       else return []

    let allCppFiles = case problemType problem of
            Stream _ _ -> solutionPath:mainFilePath:libraryCPPFiles
            Topcoder _ -> solutionPath:libraryCPPFiles
        outputPath = problemDir </> "submission.cpp"

    ret <- liftIO $
        inlineLibraryCode tempDir cmdLineOptions macrosToKeep maxConsequentEmptyLines allCppFiles outputPath

    when (ret /= 0) $
        throw . T.concat $ ["C++ inliner failed with error code ", tshow ret]

    liftIO $ copyFile outputPath $ root </> "submission.cpp"


listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively dir = do
    (files, dirs) <- listDir dir
    recList <- concat <$> mapM listDirectoryRecursively dirs
    return $ files ++ recList

