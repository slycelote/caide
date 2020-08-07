{-# LANGUAGE OverloadedStrings #-}
module Caide.CPP.CPPSimple(
      language
) where

import Control.Monad (unless)
import Control.Monad.State (liftIO)

import Data.List (intersperse)
import qualified Data.Text as T

import Filesystem (copyFile, isFile)
import Filesystem.Util (appendTextFile, writeTextFile, pathToText)
import Filesystem.Path ((</>))
import Filesystem.Path.CurrentOS (fromText)
import qualified Filesystem.Path as F

import Caide.Problem (readProblemInfo)
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
    probType <- getProbType probID
    generateSolutionFiles probType root probID

generateSolutionFiles :: ProblemType -> F.FilePath -> ProblemID -> CaideIO ()
generateSolutionFiles (Stream input output) root probID = do
    mainFileExists <- liftIO $ isFile mainProgramPath
    unless mainFileExists $ do
        mainTemplate <- getTemplate "main_template.cpp"
        liftIO $ writeTextFile mainProgramPath $
                T.unlines $ inputPreamble ++ outputPreamble ++ [mainTemplate]
    copyTemplateUnlessExists "solution_template.cpp" scaffoldPath
    copyTemplateUnlessExists "test_template.cpp" testProgramPath
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

generateSolutionFiles (Topcoder desc) root probID = do
    solutionFileExists <- liftIO $ isFile solutionPath
    unless solutionFileExists $ do
        solutionTemplate <- getTemplate "topcoder_solution_template.cpp"
        liftIO $ writeTextFile solutionPath $
            T.unlines (solutionTemplate:tcSolution)

    testFileExists <- liftIO $ isFile testProgramPath
    unless testFileExists $ do
        testTemplate <- getTemplate "test_template.cpp"
        liftIO $ writeTextFile testProgramPath $
            T.unlines $ tcTestPreamble ++ [testTemplate]

    copyTemplateUnlessExists "topcoder_serialize.h" (problemDir </> "topcoder_serialize.h")
  where
    problemDir = root </> fromText probID
    solutionPath = problemDir </> fromText (T.append probID ".cpp")
    testProgramPath = problemDir </> fromText (T.append probID "_test.cpp")

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
    , T.concat [declareValue (method {tcValueName = "solve"}), "(",
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
getProbType probID = problemType <$> readProblemInfo probID

