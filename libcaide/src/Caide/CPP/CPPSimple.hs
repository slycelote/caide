{-# LANGUAGE OverloadedStrings #-}
module Caide.CPP.CPPSimple(
      language
) where

import Control.Monad (unless)
import Control.Monad.State (liftIO)

import Data.List (intersperse)
import qualified Data.Text as T

import Filesystem (copyFile, isFile)
import Filesystem.Util (appendTextFile, writeTextFile)
import Filesystem.Path ((</>))
import Filesystem.Path.CurrentOS (fromText)
import qualified Filesystem.Path as F

import Caide.Configuration (readProblemConfig)
import Caide.Types
import Caide.Util (pathToText, readTextFile')


language :: ProgrammingLanguage
language = ProgrammingLanguage
    { generateScaffold = generateCPPScaffold
    , inlineCode = inlineCPPCode
    }

generateCPPScaffold :: ProblemID -> CaideIO ()
generateCPPScaffold probID = do
    root <- caideRoot
    probType <- getProbType probID
    generateSolutionFiles probType root probID


generateSolutionFiles :: ProblemType -> F.FilePath -> ProblemID -> CaideIO ()
generateSolutionFiles (Stream input output) root probID = do
    mainFileExists <- liftIO $ isFile mainProgramPath
    unless mainFileExists $ do
        mainTemplate <- readTextFile' mainTemplatePath
        liftIO $ writeTextFile mainProgramPath $
                T.unlines $ inputPreamble ++ outputPreamble ++ [mainTemplate]
    liftIO $ do
        solutionFileExists <- isFile scaffoldPath
        unless solutionFileExists $ copyFile scaffoldTemplatePath scaffoldPath
        testFileExists <- isFile testProgramPath
        unless testFileExists $ copyFile testTemplatePath testProgramPath
  where
    inputPreamble = case input of
        StdIn -> ["#define CAIDE_STDIN 1"]
        FileInput fileName -> [T.concat ["const char* CAIDE_IN_FILE = \"", pathToText fileName, "\";"]]

    outputPreamble = case output of
        StdOut -> ["#define CAIDE_STDOUT 1"]
        FileOutput fileName -> [T.concat ["const char* CAIDE_OUT_FILE = \"", pathToText fileName, "\";"]]

    problemDir = root </> fromText probID
    scaffoldPath    = problemDir </> fromText (T.append probID ".cpp")
    testProgramPath = problemDir </> fromText (T.append probID "_test.cpp")
    mainProgramPath = problemDir </> "main.cpp"
    scaffoldTemplatePath = root </> "templates" </> "solution_template.cpp"
    testTemplatePath     = root </> "templates" </> "test_template.cpp"
    mainTemplatePath     = root </> "templates" </> "main_template.cpp"

generateSolutionFiles (Topcoder desc) root probID = do
    solutionFileExists <- liftIO $ isFile solutionPath
    unless solutionFileExists $ do
        solutionTemplate <- readTextFile' solutionTemplatePath
        liftIO $ writeTextFile solutionPath $
            T.unlines (solutionTemplate:tcSolution)

    testFileExists <- liftIO $ isFile testProgramPath
    unless testFileExists $ do
        testTemplate <- readTextFile' testTemplatePath
        liftIO $ writeTextFile testProgramPath $
            T.unlines $ tcTestPreamble ++ [testTemplate]
  where
    problemDir = root </> fromText probID
    solutionPath = problemDir </> fromText (T.append probID ".cpp")
    testProgramPath = problemDir </> fromText (T.append probID "_test.cpp")
    solutionTemplatePath = root </> "templates" </> "topcoder_solution_template.cpp"
    testTemplatePath     = root </> "templates" </> "test_template.cpp"

    tcTestPreamble = buildTopcoderTestPreamble (tcMethod desc) (tcMethodParameters desc)
    tcSolution = buildTopcoderSolution desc



-- Note: this will fail if Topcoder ever adds a multi-argument template types
-- (like map<K, V>) because of incorrect expansion of CAIDE_TC_PARAM macro.
-- See solution in the last section here: http://kaba.hilvi.org/pastel/techniques_deduction.htm
buildTopcoderTestPreamble :: TopcoderValue -> [TopcoderValue] -> [T.Text]
buildTopcoderTestPreamble methodDesc paramsDesc =
    [ "#define CAIDE_TOPCODER 1"
    , T.append "#define CAIDE_TC_RETURN_TYPE " (cppType methodDesc)
    , "#define CAIDE_TC_PARAM_LIST \\"
    ] ++
    [T.concat ["    CAIDE_TC_PARAM(", cppType p, ", ", tcValueName p, ")\\"]
         | p <- paramsDesc] ++
    [""]

cppType :: TopcoderValue -> T.Text
cppType value = cppTypeImpl (tcValueDimension value) (tcValueType value)
  where
    cppTypeImpl 0 TCInt = "int"
    cppTypeImpl 0 TCLong = "long long"
    cppTypeImpl 0 TCDouble = "double"
    cppTypeImpl 0 TCString = "string"
    cppTypeImpl n _ | n == 0 = "unsupportedCaideType"
    cppTypeImpl 1 baseType = T.concat ["vector<", cppTypeImpl 0     baseType, ">"]
    cppTypeImpl n baseType = T.concat ["vector<", cppTypeImpl (n-1) baseType, " >"]

isNumberType :: TopcoderValue -> Bool
isNumberType value = tcValueDimension value == 0 && isBaseNumberType (tcValueType value)
  where
    isBaseNumberType TCInt = True
    isBaseNumberType TCLong = True
    isBaseNumberType TCDouble = True
    isBaseNumberType _ = False

buildTopcoderSolution :: TopcoderProblemDescriptor -> [T.Text]
buildTopcoderSolution desc =
    [ T.concat ["class ", tcClassName desc, " {"]
    , "public:"
    -- TODO: pass the signature of main solution method to inliner
    , "    /// caide keep"
    , T.concat ["    ", declareValue method, "(",
              T.intercalate ", " (map declareValue params),
              ") {"]
    , T.concat $ ["        ", declareValue (method {tcValueName = "result"})] ++ [" = 0" | isNumberType method] ++ [";"]
    , "        return result;"
    , "    }"
    , "};"
    , ""
    , ""
    , "struct CaideSolution {"
    , T.concat ["    ", declareValue (method {tcValueName = "solve"}), "(",
              T.intercalate ", " (map declareValue params),
              ", int);"]
    , "};"
    , T.concat [declareValue (method {tcValueName = "CaideSolution::solve"}), "(",
              T.intercalate ", " (map declareValue params),
              ", int) {"]
    , T.concat ["    ", tcClassName desc, " sol;"]
    , T.concat $ "    return sol." : tcValueName method : "(" :
                intersperse ", " (map tcValueName params) ++ [");"]
    , "}"
    ]
  where
    declareValue value = T.concat [cppType value, " ", tcValueName value]
    method = tcMethod desc
    params = tcMethodParameters desc


inlineCPPCode :: ProblemID -> CaideIO ()
inlineCPPCode probID = do
    root <- caideRoot
    probType <- getProbType probID

    let problemDir = root </> fromText probID
        solutionPath = problemDir </> fromText (T.append probID ".cpp")
        inlinedCodePath = problemDir </> "submission.cpp"
        mainProgramPath = problemDir </> "main.cpp"

    liftIO $ copyFile solutionPath inlinedCodePath
    case probType of
        Stream _ _ -> do
            mainCode <- readTextFile' mainProgramPath
            liftIO $ appendTextFile inlinedCodePath mainCode
        Topcoder _ -> return ()

    liftIO $ copyFile inlinedCodePath $ root </> "submission.cpp"

getProbType :: ProblemID -> CaideIO ProblemType
getProbType probID = do
    hConf <- readProblemConfig probID
    getProp hConf "problem" "type"

