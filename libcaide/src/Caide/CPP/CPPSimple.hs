module Caide.CPP.CPPSimple(
      language
) where

import Control.Monad (unless)
import Control.Monad.State (liftIO)

import Filesystem (appendTextFile, copyFile, readTextFile, isFile)
import qualified Filesystem.Path as F
import Filesystem.Path ((</>))
import Filesystem.Path.CurrentOS (decodeString)

import Caide.Types
import Caide.Util (getProblemID)

language :: ProgrammingLanguage
language = ProgrammingLanguage
    { generateScaffold = generateCPPScaffold
    , inlineCode = inlineCPPCode
    }

generateCPPScaffold :: F.FilePath -> CaideIO ()
generateCPPScaffold problemDir = do
    let probID = getProblemID problemDir
        scaffoldPath = problemDir </> decodeString (probID ++ ".cpp")
        scaffoldTemplatePath = F.parent problemDir </> decodeString "templates" </> decodeString "solution_template.cpp"
        testProgramPath = problemDir </> decodeString (probID ++ "_test.cpp")
        testTemplatePath = F.parent problemDir </> decodeString "templates" </> decodeString "test_template.cpp"
    liftIO $ do
        solutionFileExists <- isFile scaffoldPath
        unless solutionFileExists $ copyFile scaffoldTemplatePath scaffoldPath
        testFileExists <- isFile testProgramPath
        unless testFileExists $ copyFile testTemplatePath testProgramPath

inlineCPPCode :: F.FilePath -> CaideIO ()
inlineCPPCode problemDir = do
    let probID = getProblemID problemDir
        solutionPath = problemDir </> decodeString (probID ++ ".cpp")
        inlinedTemplatePath = F.parent problemDir </> decodeString "templates" </> decodeString "main_template.cpp"
        inlinedCodePath = problemDir </> decodeString "submission.cpp"
    liftIO $ do
        copyFile solutionPath inlinedCodePath
        mainCode <- readTextFile inlinedTemplatePath
        appendTextFile inlinedCodePath mainCode

