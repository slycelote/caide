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

import Caide.Configuration (readCaideConf, withDefault)
import Caide.CPP.CBinding (inlineLibraryCode)
import Caide.Paths (problemDir)
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

    hConf <- readCaideConf
    cmdLineOptions <- getProp hConf "cpp" "clang_options"
    macrosToKeep <- withDefault ["ONLINE_JUDGE"] $ getProp hConf "cpp" "keep_macros"
    maxConsequentEmptyLines <- withDefault 2 $ getProp hConf "cpp" "max_consequent_empty_lines"


    libExists <- liftIO $ isDirectory libraryDirectory
    libraryCPPFiles <- if libExists
                       then filter (`hasExtension` "cpp") <$> liftIO (listDirectoryRecursively libraryDirectory)
                       else return []

    let (allCppFiles, additionalCmdLineOptions) = case problemType problem of
            Stream _ _ -> (solutionPath:mainFilePath:libraryCPPFiles, [])
            Topcoder _ -> (solutionPath:libraryCPPFiles, [])
            LeetCodeMethod _ -> (solutionPath:libraryCPPFiles,
                                 ["-isystem", pathToText (probDir </> CPPSimple.predefinedHeadersDir),
                                  "-include", "leetcode_predefined.h"])
        identifiersToPreserve = getIdentifiersToPreserve (problemType problem)
        outputPath = probDir </> "submission.cpp"


    ret <- liftIO $
        inlineLibraryCode tempDir (cmdLineOptions <> additionalCmdLineOptions) macrosToKeep identifiersToPreserve maxConsequentEmptyLines allCppFiles outputPath

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

listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively dir = do
    (files, dirs) <- listDir dir
    recList <- concat <$> mapM listDirectoryRecursively dirs
    return $ files ++ recList

