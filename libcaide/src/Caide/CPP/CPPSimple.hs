{-# LANGUAGE OverloadedStrings #-}
module Caide.CPP.CPPSimple(
      language
) where

import Control.Monad (unless)
import Control.Monad.State (liftIO)

import qualified Data.Text as T

import Filesystem (appendTextFile, copyFile, readTextFile, writeTextFile, isFile)
import Filesystem.Path ((</>))
import Filesystem.Path.CurrentOS (fromText)

import Caide.Configuration (readProblemConfig)
import Caide.Types
import Caide.Util (pathToText)

language :: ProgrammingLanguage
language = ProgrammingLanguage
    { generateScaffold = generateCPPScaffold
    , inlineCode = inlineCPPCode
    }

generateCPPScaffold :: ProblemID -> CaideIO ()
generateCPPScaffold probID = do
    root <- caideRoot
    hConf <- readProblemConfig probID
    probType <- getProp hConf "problem" "type"

    let problemDir = root </> fromText probID
        scaffoldPath    = problemDir </> fromText (T.append probID ".cpp")
        testProgramPath = problemDir </> fromText (T.append probID "_test.cpp")
        mainProgramPath = problemDir </> "main.cpp"
        scaffoldTemplatePath = root </> "templates" </> "solution_template.cpp"
        testTemplatePath     = root </> "templates" </> "test_template.cpp"
        mainTemplatePath     = root </> "templates" </> "main_template.cpp"
        inputPreamble = case probType of
            Stream StdIn _ -> ["#define CAIDE_STDIN 1"]
            Stream (FileInput fileName) _ -> [T.concat ["const char* CAIDE_IN_FILE = \"", pathToText fileName, "\";"]]
            _ -> []
        outputPreamble = case probType of
            Stream _ StdOut -> ["#define CAIDE_STDOUT 1"]
            Stream _ (FileOutput fileName) -> [T.concat ["const char* CAIDE_OUT_FILE = \"", pathToText fileName, "\";"]]
            _ -> []

    liftIO $ do
        solutionFileExists <- isFile scaffoldPath
        unless solutionFileExists $ copyFile scaffoldTemplatePath scaffoldPath
        testFileExists <- isFile testProgramPath
        unless testFileExists $ copyFile testTemplatePath testProgramPath
        mainFileExists <- isFile mainProgramPath
        unless mainFileExists $ do
            mainTemplate <- readTextFile mainTemplatePath
            writeTextFile mainProgramPath $ T.unlines $ inputPreamble ++ outputPreamble ++ [mainTemplate]

inlineCPPCode :: ProblemID -> CaideIO ()
inlineCPPCode probID = do
    root <- caideRoot
    let problemDir = root </> fromText probID
        solutionPath = problemDir </> fromText (T.append probID ".cpp")
        inlinedCodePath = problemDir </> "submission.cpp"
        mainProgramPath = problemDir </> "main.cpp"
    liftIO $ do
        copyFile solutionPath inlinedCodePath
        mainCode <- readTextFile mainProgramPath
        appendTextFile inlinedCodePath mainCode

