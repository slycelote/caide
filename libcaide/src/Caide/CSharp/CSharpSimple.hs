{-# LANGUAGE OverloadedStrings #-}
module Caide.CSharp.CSharpSimple(
      language
) where

import Control.Monad (unless)
import Control.Monad.State (liftIO)

import qualified Data.Text as T

import Filesystem (copyFile, isFile)
import Filesystem.Util (appendTextFile, writeTextFile, pathToText)
import qualified Filesystem.Path as F
import Filesystem.Path ((</>))
import Filesystem.Path.CurrentOS (fromText)

import Caide.Configuration (readProblemConfig)
import Caide.Types
import Caide.Util (readTextFile')


language :: ProgrammingLanguage
language = ProgrammingLanguage
    { generateScaffold = generateCSharpScaffold
    , inlineCode = inlineCSharpCode
    }

generateCSharpScaffold :: ProblemID -> CaideIO ()
generateCSharpScaffold probID = do
    root <- caideRoot
    hConf <- readProblemConfig probID
    probType <- getProp hConf "problem" "type"

    let problemDir = root </> fromText probID
        scaffoldPath = problemDir </> fromText (T.append probID ".cs")
        scaffoldTemplatePath = F.parent problemDir </> "templates" </> "solution_template.cs"
        testProgramPath = problemDir </> fromText (T.append probID "_test.cs")
        testTemplatePath = F.parent problemDir </> "templates" </> "test_template.cs"
        mainProgramPath = problemDir </> "main.cs"
        mainTemplatePath = root </> "templates" </> "main_template.cs"
        inputPreamble = case probType of
            Stream StdIn _ -> "    public const string InputFile = null;"
            Stream (FileInput fileName) _ -> T.concat ["    public const string InputFile = \"", pathToText fileName, "\";"]
            _ -> ""
        outputPreamble = case probType of
            Stream _ StdOut -> "    public const string OutputFile = null;"
            Stream _ (FileOutput fileName) -> T.concat ["    public const string OutputFile = \"", pathToText fileName, "\";"]
            _ -> ""
        caideConstants = if T.null inputPreamble && T.null outputPreamble
            then ""
            else T.unlines ["class CaideConstants {", inputPreamble, outputPreamble, "}"]

    mainFileExists <- liftIO $ isFile mainProgramPath
    unless mainFileExists $ do
        mainTemplate <- readTextFile' mainTemplatePath
        liftIO $ writeTextFile mainProgramPath $ T.append caideConstants mainTemplate

    liftIO $ do
        solutionFileExists <- isFile scaffoldPath
        unless solutionFileExists $ copyFile scaffoldTemplatePath scaffoldPath
        testFileExists <- isFile testProgramPath
        unless testFileExists $ copyFile testTemplatePath testProgramPath


inlineCSharpCode :: ProblemID -> CaideIO ()
inlineCSharpCode probID = do
    root <- caideRoot
    let problemDir = root </> fromText probID
        solutionPath = problemDir </> fromText (T.append probID ".cs")
        mainProgramPath = problemDir </> "main.cs"
        inlinedCodePath = problemDir </> "submission.cs"

    mainCode <- readTextFile' mainProgramPath
    liftIO $ do
        copyFile solutionPath inlinedCodePath
        appendTextFile inlinedCodePath mainCode
        copyFile inlinedCodePath $ root </> "submission.cs"

