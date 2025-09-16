{-# LANGUAGE CPP, OverloadedStrings #-}
module Caide.CPP.CPP(
      language
) where

import Control.Monad.Extended (when, liftIO)
import qualified Data.Text as T

import Prelude hiding (FilePath)
import Filesystem (copyFile, isDirectory)
import qualified Filesystem.Path.CurrentOS as FS
import Filesystem.Path.CurrentOS ((</>), FilePath, hasExtension)

import Filesystem.Util (listDir, pathToText)

import qualified Caide.CPP.CPPSimple as CPPSimple

import Caide.CPP.CBinding (inlineLibraryCode)
import Caide.Paths (problemDir)
import qualified Caide.Settings as Settings
import qualified Caide.Problem as Problem
import Caide.Types
import Caide.Util (tshow)


language :: ProgrammingLanguage
language = CPPSimple.language {inlineCode = inlineCPPCode}


inlineCPPCode :: ProblemID -> CaideIO ()
inlineCPPCode probID = do
    root <- caideRoot
    problem <- Problem.readProblemInfo probID
    let probDir = problemDir root probID
        solutionPath = probDir </> FS.fromText (probID <> ".cpp")
        mainFilePath = probDir </> "main.cpp"
        tempDir = probDir </> ".caideproblem"
        libraryDirectory = root </> "cpplib"

    settings <- Settings.cppSettings <$> caideSettings
    let cmdLineOptions = Settings.clangOptions settings
        macrosToKeep = Settings.keepMacros settings
        maxConsecutiveEmptyLines = Settings.maxConsecutiveEmptyLines settings

    libExists <- liftIO $ isDirectory libraryDirectory
    libraryCPPFiles <- if libExists
                       then filter (`hasExtension` "cpp") <$> liftIO (listDirectoryRecursively libraryDirectory)
                       else return []

    let (allCppFiles, additionalCmdLineOptions) = case problemType problem of
            Stream{} -> (solutionPath:mainFilePath:libraryCPPFiles, [])
            Topcoder _ -> (solutionPath:libraryCPPFiles, [])
            LeetCodeMethod _ -> (solutionPath:libraryCPPFiles,
                                 ["-isystem", pathToText (probDir </> CPPSimple.predefinedHeadersDir),
                                  "-include", "leetcode_predefined.h"])
            LeetCodeClass{} -> (solutionPath:libraryCPPFiles,
                                ["-isystem", pathToText (probDir </> CPPSimple.predefinedHeadersDir),
                                 "-include", "leetcode_predefined.h"])
        identifiersToPreserve = getIdentifiersToPreserve (problemType problem)
        outputPath = probDir </> "submission.cpp"


    ret <- liftIO $
        inlineLibraryCode tempDir (cmdLineOptions <> additionalCmdLineOptions) macrosToKeep identifiersToPreserve maxConsecutiveEmptyLines allCppFiles outputPath

    when (ret /= 0) $
        throw $ "C++ inliner failed with error code " <> tshow ret

    liftIO $ copyFile outputPath $ root </> "submission.cpp"

getIdentifiersToPreserve :: ProblemType -> [T.Text]
getIdentifiersToPreserve (Stream _ _) = []
getIdentifiersToPreserve (Topcoder desc) =
    [ tcClassName desc <> "::" <> tcClassName desc -- constructor
    , tcClassName desc <> "::" <> tcValueName (tcMethod (tcSingleMethod desc)) -- main method
    ]
getIdentifiersToPreserve (LeetCodeMethod method) =
    [ defaultLeetCodeClassName <> "::" <> defaultLeetCodeClassName -- constructor
    , defaultLeetCodeClassName <> "::" <> tcValueName (tcMethod method) -- main method
    ]

getIdentifiersToPreserve (LeetCodeClass className _ methods) =
    (className <> "::" <> className) -- constructor
    : [className <> "::" <> tcValueName (tcMethod method) | method <- methods]

listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively dir = do
    (files, dirs) <- listDir dir
    recList <- concat <$> mapM listDirectoryRecursively dirs
    return $ files ++ recList

