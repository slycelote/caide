{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Caide.CSharp.CSharpSimple(
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
    { generateScaffold = generateCSharpScaffold
    , inlineCode = inlineCSharpCode
    }

generateCSharpScaffold :: ProblemID -> CaideIO ()
generateCSharpScaffold probID = do
    root <- caideRoot
    problem <- readProblemInfo probID
    problemState <- readProblemState probID

    generateSolutionAndMain problem problemState

    let problemDir = root </> fromText probID
        testProgramPath = problemDir </> fromText (probID <> "_test.cs")
        testerCode = generateTesterCode problem problemState

    testFileExists <- liftIO $ isFile testProgramPath
    unless testFileExists $ do
        testTemplate <- getTemplate "test_template.cs"
        liftIO $ writeTextFile testProgramPath $ testTemplate <> testerCode

solutionTemplates :: NonEmpty.NonEmpty (Text, Text)
solutionTemplates =
    NonEmpty.fromList [ ("solution.cs", $(embedStringFile "src/Caide/CSharp/solution.cs.mustache"))
                      , ("cstype", $(embedStringFile "src/Caide/CSharp/cstype.mustache"))
                      ]

mainTemplates :: NonEmpty.NonEmpty (Text, Text)
mainTemplates =
    NonEmpty.fromList [ ("main.cs", $(embedStringFile "src/Caide/CSharp/main.cs.mustache"))
                      ]

generateSolutionAndMain :: Problem -> ProblemState -> CaideIO ()
generateSolutionAndMain problem@Problem{problemType=Stream _ _} state = do
    root <- caideRoot
    let probID = problemId problem
        problemDir = root </> fromText probID
        solutionPath = problemDir </> fromText (probID <> ".cs")
        mainProgramPath = problemDir </> "main.cs"

    mainFileExists <- liftIO $ isFile mainProgramPath
    unless mainFileExists $ do
        mainTemplate <- getTemplate "main_template.cs"
        liftIO $ writeTextFile mainProgramPath $
            renderTemplate mainTemplates problem state <> "\n" <> mainTemplate

    copyTemplateUnlessExists "solution_template.cs" solutionPath

generateSolutionAndMain problem@Problem{problemType=Topcoder _} state =
    generateSolutionAndMainForTopcoder problem state

generateSolutionAndMain problem@Problem{problemType=LeetCodeMethod _} state =
    generateSolutionAndMainForTopcoder problem state

generateSolutionAndMainForTopcoder :: Problem -> ProblemState -> CaideIO ()
generateSolutionAndMainForTopcoder problem state = do
    root <- caideRoot
    let probID = problemId problem
        problemDir = root </> fromText probID
        solutionPath = problemDir </> fromText (probID <> ".cs")

    solutionFileExists <- liftIO $ isFile solutionPath
    unless solutionFileExists $ do
        userSolutionTemplate <- getTemplate "topcoder_solution_template.cs"
        liftIO $ writeTextFile solutionPath $
            userSolutionTemplate <> "\n" <> renderTemplate solutionTemplates problem state

inlineCSharpCode :: ProblemID -> CaideIO ()
inlineCSharpCode probID = do
    root <- caideRoot
    probType <- problemType <$> readProblemInfo probID
    let problemDir = root </> fromText probID
        solutionPath = problemDir </> fromText (probID <> ".cs")
        mainProgramPath = problemDir </> "main.cs"
        inlinedCodePath = problemDir </> "submission.cs"

    liftIO $ copyFile solutionPath inlinedCodePath
    case probType of
        Stream _ _ -> do
            mainCode <- readTextFile' mainProgramPath
            liftIO $ appendTextFile inlinedCodePath mainCode
        Topcoder _ -> return ()
        LeetCodeMethod _ -> return ()
        -- LeetCodeClass _ _ _ -> return ()

    liftIO $ copyFile inlinedCodePath $ root </> "submission.cs"

testerTemplates :: NonEmpty.NonEmpty (Text, Text)
testerTemplates =
    NonEmpty.fromList [ ("tester.cs", $(embedStringFile "src/Caide/CSharp/tester.cs.mustache"))
                      , ("cstype", $(embedStringFile "src/Caide/CSharp/cstype.mustache"))
                      , ("serializer", $(embedStringFile "src/Caide/CSharp/serializer.mustache"))
                      ]

generateTesterCode :: Problem -> ProblemState -> Text
generateTesterCode = renderTemplate testerTemplates

renderTemplate :: NonEmpty.NonEmpty (Text, Text) -> Problem -> ProblemState -> Text
renderTemplate templates problem state = let
    json = jsonEncodeProblem problem state
    in case compileAndRender templates json of
        Left err -> err
        Right (_warnings, result) -> result

