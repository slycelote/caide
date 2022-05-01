{-# LANGUAGE NamedFieldPuns, OverloadedStrings, TemplateHaskell #-}
module Caide.CPP.CPPSimple(
      language

      -- | Directory with headers containing things predefined at an online judge.
      -- Relative to problem directory.
    , predefinedHeadersDir
) where

import Control.Monad.Extended (forM_, liftIO, unless, unlessM, whenJust)

import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)

import Data.FileEmbed (embedStringFile)

import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem as FS
import Filesystem.Path.CurrentOS ((</>))
import Filesystem.Util (appendTextFile, writeTextFile)
import Caide.Util (readTextFile')

import Caide.Logger (logDebug)
import Caide.MustacheUtil (compileAndRender)
import Caide.Paths (problemDir)
import Caide.Problem (ProblemState, jsonEncodeProblem, readProblemInfo, readProblemState)
import Caide.Templates (copyTemplateUnlessExists, getTemplate)
import Caide.Types


language :: ProgrammingLanguage
language = ProgrammingLanguage
    { generateScaffold = generateCPPScaffold
    , inlineCode = inlineCPPCode
    }

generateCPPScaffold :: ProblemID -> CaideIO ()
generateCPPScaffold probID = do
    root <- caideRoot
    problem <- readProblemInfo probID
    problemState <- readProblemState probID

    generateSolutionAndMain problem problemState

    let probDir = problemDir root probID
        testProgramPath = probDir </> FS.fromText (probID <> "_test.cpp")

    testerCode <- generateTesterCode problem problemState
    testFileExists <- liftIO $ FS.isFile testProgramPath
    unless testFileExists $ do
        testTemplate <- getTemplate "test_template.cpp"
        liftIO $ writeTextFile testProgramPath $ testerCode <> "\n" <> testTemplate

predefinedHeadersDir :: FS.FilePath
predefinedHeadersDir = "predefined"

probIdAndDir :: Problem -> CaideIO (ProblemID, FS.FilePath)
probIdAndDir problem = do
    root <- caideRoot
    let probID = problemId problem
    return (probID, problemDir root probID)

generateSolutionAndMain :: Problem -> ProblemState -> CaideIO ()
generateSolutionAndMain problem@Problem{problemType=Stream _ _} state = do
    (probID, probDir) <- probIdAndDir problem
    let solutionPath = probDir </> FS.fromText (probID <> ".cpp")
        mainProgramPath = probDir </> "main.cpp"

    unlessM (liftIO $ FS.isFile mainProgramPath) $ do
        userMainTemplate <- getTemplate "main_template.cpp"
        rendered <- renderTemplates ["main.cpp"] problem state
        let [(_,mainTemplate)] = rendered
        liftIO $ writeTextFile mainProgramPath $
            mainTemplate <> "\n" <> userMainTemplate

    copyTemplateUnlessExists "solution_template.cpp" solutionPath


generateSolutionAndMain problem@Problem{problemType=Topcoder _} state =
    generateSolutionAndMainForTopcoder problem state

generateSolutionAndMain problem@Problem{problemType=LeetCodeMethod _} state =
    generateSolutionAndMainForLeetCode problem state

generateSolutionAndMain problem@Problem{problemType=LeetCodeClass _ _ _} state =
    generateSolutionAndMainForLeetCode problem state


generateSolutionAndMainForLeetCode :: Problem -> ProblemState -> CaideIO ()
generateSolutionAndMainForLeetCode problem state = do
    generateSolutionAndMainForTopcoder problem state

    (_, probDir) <- probIdAndDir problem
    let predefDir = probDir </> predefinedHeadersDir
        leetcodePredef = predefDir </> "leetcode_predefined.h"
    liftIO $ do
        FS.createTree predefDir
        unlessM (FS.isFile leetcodePredef) $
            writeTextFile leetcodePredef leetcodePredefText

leetcodePredefText :: Text
leetcodePredefText = $(embedStringFile "src/Caide/CPP/leetcode_predefined.h")


generateSolutionAndMainForTopcoder :: Problem -> ProblemState -> CaideIO ()
generateSolutionAndMainForTopcoder problem state = do
    (probID, probDir) <- probIdAndDir problem
    let solutionPath = probDir </> FS.fromText (probID <> ".cpp")

    solutionFileExists <- liftIO $ FS.isFile solutionPath
    unless solutionFileExists $ do
        userSolutionTemplate <- getTemplate "topcoder_solution_template.cpp"
        rendered <- renderTemplates ["solution.cpp"] problem state
        let [(_,solutionTemplate)] = rendered
        liftIO $ writeTextFile solutionPath $
            userSolutionTemplate <> "\n" <> solutionTemplate


allTemplates :: NonEmpty.NonEmpty (Text, Text)
allTemplates =
    NonEmpty.fromList [ ("tester.cpp", $(embedStringFile "src/Caide/CPP/tester.cpp.mustache"))
                      , ("main.cpp", $(embedStringFile "src/Caide/CPP/main.cpp.mustache"))
                      , ("solution.cpp", $(embedStringFile "src/Caide/CPP/solution.cpp.mustache"))
                      , ("custom_checker.h", $(embedStringFile "src/Caide/CPP/custom_checker.h.mustache"))
                      , ("class_tester.h", $(embedStringFile "src/Caide/CPP/class_tester.h.mustache"))
                      , ("class_tester_impl.h", $(embedStringFile "src/Caide/CPP/class_tester_impl.h.mustache"))
                      , ("cpptype", $(embedStringFile "src/Caide/CPP/cpptype.mustache"))
                      ]


generateTesterCode :: Problem -> ProblemState -> CaideIO Text
generateTesterCode problem state = do
    (_, probDir) <- probIdAndDir problem
    let testerTemplates = ["tester.cpp", "custom_checker.h"] ++
            case problemType problem of
                Stream _ _ -> []
                Topcoder _ -> ["class_tester.h", "class_tester_impl.h"]
                LeetCodeMethod _ -> ["class_tester.h", "class_tester_impl.h"]
                LeetCodeClass _ _ _ -> ["class_tester.h", "class_tester_impl.h"]

    rendered <- renderTemplates testerTemplates problem state
    let ((_, testerCode):rest) = rendered
    liftIO $ writeFiles probDir rest
    copyTemplateUnlessExists "test_util.h" (probDir </> "test_util.h")
    return testerCode


writeFiles :: FS.FilePath -> [(Text, Text)] -> IO ()
writeFiles dir renderedTemplates =
    forM_ renderedTemplates $ \(name, renderedText) -> do
        let path = dir </> FS.fromText name
        unlessM (FS.isFile path) $ writeTextFile path renderedText

renderTemplates :: [Text] -> Problem -> ProblemState -> CaideIO [(Text, Text)]
renderTemplates primaryTemplateNames problem state = do
    let json = jsonEncodeProblem problem state
    case compileAndRender allTemplates primaryTemplateNames json of
        Left err -> throw err
        Right (warnings, res) -> do
            whenJust warnings logDebug
            return $ zip primaryTemplateNames res


inlineCPPCode :: ProblemID -> CaideIO ()
inlineCPPCode probID = do
    root <- caideRoot
    probType <- problemType <$> readProblemInfo probID
    let probDir = problemDir root probID
        solutionPath = probDir </> FS.fromText (probID <> ".cpp")
        inlinedCodePath = probDir </> "submission.cpp"
        mainProgramPath = probDir </> "main.cpp"

    liftIO $ FS.copyFile solutionPath inlinedCodePath
    case probType of
        Stream _ _ -> do
            mainCode <- readTextFile' mainProgramPath
            liftIO $ appendTextFile inlinedCodePath mainCode
        Topcoder _ -> return ()
        LeetCodeMethod _ -> return ()
        LeetCodeClass _ _ _ -> return ()

    liftIO $ FS.copyFile inlinedCodePath $ root </> "submission.cpp"

