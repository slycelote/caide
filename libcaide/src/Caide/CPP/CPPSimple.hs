module Caide.CPP.CPPSimple(
    language
) where

import qualified Data.Text as T

import Filesystem (appendTextFile, copyFile)
import qualified Filesystem.Path as F
import Filesystem.Path ((</>),)
import Filesystem.Path.CurrentOS (decodeString)

import Caide.Types (ProgrammingLanguage(..))
import Caide.Util (getProblemID)

generateCPPScaffold :: F.FilePath -> IO ()
generateCPPScaffold problemDir = do
    let probID = getProblemID problemDir
        scaffoldPath = problemDir </> decodeString (probID ++ ".cpp")
        scaffoldTemplatePath = F.parent problemDir </> decodeString "solution_template.cpp"
    copyFile scaffoldTemplatePath scaffoldPath

generateCPPTestProgram :: F.FilePath -> IO ()
generateCPPTestProgram problemDir = do
    let probID = getProblemID problemDir
        testProgramPath = problemDir </> decodeString (probID ++ "_test.cpp")
        testTemplatePath = F.parent problemDir </> decodeString "test_template.cpp"
    copyFile testTemplatePath testProgramPath

inlineCPPCode :: F.FilePath -> IO ()
inlineCPPCode problemDir = do
    let probID = getProblemID problemDir
        solutionPath = problemDir </> decodeString (probID ++ ".cpp")
        inlinedCodePath = problemDir </> decodeString "main.cpp"
    copyFile solutionPath inlinedCodePath
    appendTextFile inlinedCodePath $ T.pack "\r\n\r\nint main() {solve(std::cin, std::cout);return 0;}\r\n"

language :: ProgrammingLanguage
language = ProgrammingLanguage
    { generateScaffold = generateCPPScaffold
    , generateTestProgram = generateCPPTestProgram
    , inlineCode = inlineCPPCode
    }
