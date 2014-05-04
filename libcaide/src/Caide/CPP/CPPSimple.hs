module Caide.CPP.CPPSimple(
      language
) where

import Control.Monad (unless)

import Filesystem (appendTextFile, copyFile, readTextFile, isFile)
import qualified Filesystem.Path as F
import Filesystem.Path ((</>),)
import Filesystem.Path.CurrentOS (decodeString)

import Caide.Types (ProgrammingLanguage(..))
import Caide.Util (getProblemID)

generateCPPScaffold :: F.FilePath -> IO ()
generateCPPScaffold problemDir = do
    let probID = getProblemID problemDir
        scaffoldPath = problemDir </> decodeString (probID ++ ".cpp")
        scaffoldTemplatePath = F.parent problemDir </> decodeString "templates" </> decodeString "solution_template.cpp"
    fileExists <- isFile scaffoldPath
    unless fileExists $ copyFile scaffoldTemplatePath scaffoldPath

generateCPPTestProgram :: F.FilePath -> IO ()
generateCPPTestProgram problemDir = do
    let probID = getProblemID problemDir
        testProgramPath = problemDir </> decodeString (probID ++ "_test.cpp")
        testTemplatePath = F.parent problemDir </> decodeString "templates" </> decodeString "test_template.cpp"
    fileExists <- isFile testProgramPath
    -- TODO: Replace CAIDE_EXE token with full path to caide executable in test template
    unless fileExists $ copyFile testTemplatePath testProgramPath

inlineCPPCode :: F.FilePath -> IO ()
inlineCPPCode problemDir = do
    let probID = getProblemID problemDir
        solutionPath = problemDir </> decodeString (probID ++ ".cpp")
        inlinedTemplatePath = F.parent problemDir </> decodeString "templates" </> decodeString "main_template.cpp"
        inlinedCodePath = problemDir </> decodeString "main.cpp"
    copyFile solutionPath inlinedCodePath
    mainCode <- readTextFile inlinedTemplatePath
    appendTextFile inlinedCodePath mainCode

language :: ProgrammingLanguage
language = ProgrammingLanguage
    { generateScaffold = generateCPPScaffold
    , generateTestProgram = generateCPPTestProgram
    , inlineCode = inlineCPPCode
    }
