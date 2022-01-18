{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Caide.CPP.CPPSimple(
      language
) where

import Control.Monad (unless)
import Control.Monad.State (liftIO)

import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)

import Data.FileEmbed (embedStringFile)

import Filesystem (copyFile, isFile)
import Filesystem.Util (appendTextFile, writeTextFile)
import Filesystem.Path ((</>))
import Filesystem.Path.CurrentOS (fromText)

import Caide.MustacheUtil (compileAndRender)
import Caide.Problem (ProblemState, jsonEncodeProblem, readProblemInfo, readProblemState)
import Caide.Templates (copyTemplateUnlessExists, getTemplate)
import Caide.Types
import Caide.Util (readTextFile')


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

    let problemDir = root </> fromText probID
        testProgramPath = problemDir </> fromText (probID <> "_test.cpp")
        testerCode = generateTesterCode problem problemState

    testFileExists <- liftIO $ isFile testProgramPath
    unless testFileExists $ do
        testTemplate <- getTemplate "test_template.cpp"
        liftIO $ writeTextFile testProgramPath $ testerCode <> testTemplate

solutionTemplates :: NonEmpty.NonEmpty (Text, Text)
solutionTemplates =
    NonEmpty.fromList [ ("solution.cpp", $(embedStringFile "src/Caide/CPP/solution.cpp.mustache"))
                      , ("cpptype", $(embedStringFile "src/Caide/CPP/cpptype.mustache"))
                      , ("typeinit", $(embedStringFile "src/Caide/CPP/typeinit.mustache"))
                      ]

mainTemplates :: NonEmpty.NonEmpty (Text, Text)
mainTemplates =
    NonEmpty.fromList [ ("main.cpp", $(embedStringFile "src/Caide/CPP/main.cpp.mustache"))
                      ]

generateSolutionAndMain :: Problem -> ProblemState -> CaideIO ()
generateSolutionAndMain problem@Problem{problemType=Stream _ _} state = do
    root <- caideRoot
    let probID = problemId problem
        problemDir = root </> fromText probID
        solutionPath = problemDir </> fromText (probID <> ".cpp")
        mainProgramPath = problemDir </> "main.cpp"

    mainFileExists <- liftIO $ isFile mainProgramPath
    unless mainFileExists $ do
        mainTemplate <- getTemplate "main_template.cpp"
        liftIO $ writeTextFile mainProgramPath $
            renderTemplate mainTemplates problem state <> "\n" <> mainTemplate

    copyTemplateUnlessExists "solution_template.cpp" solutionPath

generateSolutionAndMain problem@Problem{problemType=Topcoder _} state = do
    root <- caideRoot
    let probID = problemId problem
        problemDir = root </> fromText probID
        solutionPath = problemDir </> fromText (probID <> ".cpp")

    solutionFileExists <- liftIO $ isFile solutionPath
    unless solutionFileExists $ do
        userSolutionTemplate <- getTemplate "topcoder_solution_template.cpp"
        liftIO $ writeTextFile solutionPath $
            userSolutionTemplate <> "\n" <> renderTemplate solutionTemplates problem state

    copyTemplateUnlessExists "topcoder_serialize.h" (problemDir </> "topcoder_serialize.h")


inlineCPPCode :: ProblemID -> CaideIO ()
inlineCPPCode probID = do
    root <- caideRoot
    probType <- problemType <$> readProblemInfo probID
    let problemDir = root </> fromText probID
        solutionPath = problemDir </> fromText (probID <> ".cpp")
        inlinedCodePath = problemDir </> "submission.cpp"
        mainProgramPath = problemDir </> "main.cpp"

    liftIO $ copyFile solutionPath inlinedCodePath
    case probType of
        Stream _ _ -> do
            mainCode <- readTextFile' mainProgramPath
            liftIO $ appendTextFile inlinedCodePath mainCode
        Topcoder _ -> return ()
        -- LeetCodeMethod _ -> return ()
        -- LeetCodeClass _ _ _ -> return ()

    liftIO $ copyFile inlinedCodePath $ root </> "submission.cpp"

testerTemplates :: NonEmpty.NonEmpty (Text, Text)
testerTemplates =
    NonEmpty.fromList [ ("tester.cpp", $(embedStringFile "src/Caide/CPP/tester.cpp.mustache"))
                      , ("cpptype", $(embedStringFile "src/Caide/CPP/cpptype.mustache"))
                      ]

generateTesterCode :: Problem -> ProblemState -> Text
generateTesterCode = renderTemplate testerTemplates

renderTemplate :: NonEmpty.NonEmpty (Text, Text) -> Problem -> ProblemState -> Text
renderTemplate templates problem state = let
    json = jsonEncodeProblem problem state
    in case compileAndRender templates json of
        Left err -> err
        Right (_warnings, result) -> result
