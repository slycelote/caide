{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Caide.CSharp.CSharpSimple(
      language
) where

import Control.Monad.Extended (liftIO, unlessM, whenJust)

import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)

import Data.FileEmbed (embedStringFile)

import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FS
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
    { generateScaffold = generateCSharpScaffold
    , inlineCode = inlineCSharpCode
    }


generateCSharpScaffold :: ProblemID -> CaideIO ()
generateCSharpScaffold probID = do
    root <- caideRoot
    problem <- readProblemInfo probID
    problemState <- readProblemState probID

    generateSolutionAndMain problem problemState

    let probDir = problemDir root probID
        testProgramPath = probDir </> FS.fromText (probID <> "_test.cs")

    testerCode <- renderTemplate "tester.cs" problem problemState
    unlessM (liftIO $ FS.isFile testProgramPath) $ do
        userTestTemplate <- getTemplate "test_template.cs"
        liftIO $ writeTextFile testProgramPath $ testerCode <> "\n" <> userTestTemplate

probIdAndDir :: Problem -> CaideIO (ProblemID, FS.FilePath)
probIdAndDir problem = do
    root <- caideRoot
    let probID = problemId problem
    return (probID, problemDir root probID)

generateSolutionAndMain :: Problem -> ProblemState -> CaideIO ()
generateSolutionAndMain problem@Problem{problemType=Stream _ _} state = do
    (probID, probDir) <- probIdAndDir problem
    let solutionPath = probDir </> FS.fromText (probID <> ".cs")
        mainProgramPath = probDir </> "main.cs"

    unlessM (liftIO $ FS.isFile mainProgramPath) $ do
        userMainTemplate <- getTemplate "main_template.cs"
        mainTemplate <- renderTemplate "main.cs" problem state
        liftIO $ writeTextFile mainProgramPath $
            mainTemplate <> "\n" <> userMainTemplate

    copyTemplateUnlessExists "solution_template.cs" solutionPath


generateSolutionAndMain problem@Problem{problemType=Topcoder _} state =
    generateSolutionAndMainForTopcoder problem state

generateSolutionAndMain problem@Problem{problemType=LeetCodeMethod _} state =
    generateSolutionAndMainForTopcoder problem state


generateSolutionAndMainForTopcoder :: Problem -> ProblemState -> CaideIO ()
generateSolutionAndMainForTopcoder problem state = do
    (probID, probDir) <- probIdAndDir problem
    let solutionPath = probDir </> FS.fromText (probID <> ".cs")

    unlessM (liftIO $ FS.isFile solutionPath) $ do
        userSolutionTemplate <- getTemplate "topcoder_solution_template.cs"
        solutionTemplate <- renderTemplate "solution.cs" problem state
        liftIO $ writeTextFile solutionPath $
            userSolutionTemplate <> "\n" <> solutionTemplate


renderTemplate :: Text -> Problem -> ProblemState -> CaideIO Text
renderTemplate primaryTemplateName problem state = do
    let json = jsonEncodeProblem problem state
    case compileAndRender allTemplates [primaryTemplateName] json of
        Left err -> throw err
        Right (warnings, res) -> do
            whenJust warnings logDebug
            return $ head res

allTemplates :: NonEmpty.NonEmpty (Text, Text)
allTemplates = NonEmpty.fromList
             [ ("tester.cs", $(embedStringFile "src/Caide/CSharp/tester.cs.mustache"))
             , ("solution.cs", $(embedStringFile "src/Caide/CSharp/solution.cs.mustache"))
             , ("main.cs", $(embedStringFile "src/Caide/CSharp/main.cs.mustache"))
             , ("cstype", $(embedStringFile "src/Caide/CSharp/cstype.mustache"))
             , ("serializer", $(embedStringFile "src/Caide/CSharp/serializer.mustache"))
             ]


inlineCSharpCode :: ProblemID -> CaideIO ()
inlineCSharpCode probID = do
    root <- caideRoot
    probType <- problemType <$> readProblemInfo probID
    let probDir = problemDir root probID
        solutionPath = probDir </> FS.fromText (probID <> ".cs")
        mainProgramPath = probDir </> "main.cs"
        inlinedCodePath = probDir </> "submission.cs"

    liftIO $ FS.copyFile solutionPath inlinedCodePath
    case probType of
        Stream _ _ -> do
            mainCode <- readTextFile' mainProgramPath
            liftIO $ appendTextFile inlinedCodePath mainCode
        Topcoder _ -> return ()
        LeetCodeMethod _ -> return ()
        -- LeetCodeClass _ _ _ -> return ()

    liftIO $ FS.copyFile inlinedCodePath $ root </> "submission.cs"

