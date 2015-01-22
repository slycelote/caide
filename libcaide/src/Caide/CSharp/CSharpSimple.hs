{-# LANGUAGE OverloadedStrings #-}
module Caide.CSharp.CSharpSimple (
      language
) where

import Control.Monad (unless)
import Control.Monad.State (liftIO)

import qualified Data.Text as T

import Filesystem (appendTextFile, copyFile, readTextFile, isFile)
import qualified Filesystem.Path as F
import Filesystem.Path ((</>))
import Filesystem.Path.CurrentOS (fromText)

import Caide.Types
import Caide.Util (getProblemID)

language :: ProgrammingLanguage
language = ProgrammingLanguage
    { generateScaffold = generateCSharpScaffold
    , inlineCode = inlineCSharpCode
    }

generateCSharpScaffold :: F.FilePath -> CaideIO ()
generateCSharpScaffold problemDir = do
    let probID = getProblemID problemDir
        scaffoldPath = problemDir </> fromText (T.append probID ".cs")
        scaffoldTemplatePath = F.parent problemDir </> "templates" </> "solution_template.cs"
        testProgramPath = problemDir </> fromText (T.append probID "_test.cs")
        testTemplatePath = F.parent problemDir </> "templates" </> "test_template.cs"
    liftIO $ do
        solutionFileExists <- isFile scaffoldPath
        unless solutionFileExists $ copyFile scaffoldTemplatePath scaffoldPath
        testFileExists <- isFile testProgramPath
        unless testFileExists $ copyFile testTemplatePath testProgramPath

inlineCSharpCode :: F.FilePath -> CaideIO ()
inlineCSharpCode problemDir = do
    let probID = getProblemID problemDir
        solutionPath = problemDir </> fromText (T.append probID ".cs")
        inlinedTemplatePath = F.parent problemDir </> "templates" </> "main_template.cs"
        inlinedCodePath = problemDir </> "submission.cs"
    liftIO $ do
        copyFile solutionPath inlinedCodePath
        mainCode <- readTextFile inlinedTemplatePath
        appendTextFile inlinedCodePath mainCode

