{-# LANGUAGE OverloadedStrings #-}
module Caide.CSharp.CSharpSimple(
      language
) where

import Control.Monad (unless)
import Control.Monad.State (liftIO)

import Data.List (intersperse)
import qualified Data.Text as T

import Filesystem (copyFile, isFile)
import Filesystem.Util (appendTextFile, writeTextFile, pathToText)
import qualified Filesystem.Path as F
import Filesystem.Path ((</>))
import Filesystem.Path.CurrentOS (fromText)

import Caide.Configuration (readProblemConfig)
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
    hConf <- readProblemConfig probID
    probType <- getProp hConf "problem" "type"

    generateSolutionFiles probType root probID

    let problemDir = root </> fromText probID
        testProgramPath = problemDir </> fromText (T.append probID "_test.cs")
        testerCode = generateTesterCode probType

    testFileExists <- liftIO $ isFile testProgramPath
    unless testFileExists $ do
        testTemplate <- getTemplate "test_template.cs"
        liftIO $ writeTextFile testProgramPath $ T.append testTemplate $ T.unlines testerCode

generateSolutionFiles :: ProblemType -> F.FilePath -> ProblemID -> CaideIO ()
generateSolutionFiles (Stream input output) root probID = do
    mainFileExists <- liftIO $ isFile mainProgramPath
    unless mainFileExists $ do
        mainTemplate <- getTemplate "main_template.cs"
        liftIO $ writeTextFile mainProgramPath $ T.append caideConstants mainTemplate

    copyTemplateUnlessExists "solution_template.cs" scaffoldPath
  where
    problemDir = root </> fromText probID
    scaffoldPath = problemDir </> fromText (T.append probID ".cs")
    mainProgramPath = problemDir </> "main.cs"
    inputPreamble = case input of
        StdIn -> "    public const string InputFile = null;"
        FileInput fileName -> T.concat ["    public const string InputFile = \"", pathToText fileName, "\";"]
    outputPreamble = case output of
        StdOut -> "    public const string OutputFile = null;"
        FileOutput fileName -> T.concat ["    public const string OutputFile = \"", pathToText fileName, "\";"]
    caideConstants = if T.null inputPreamble && T.null outputPreamble
        then ""
        else T.unlines ["class CaideConstants {", inputPreamble, outputPreamble, "}"]


generateSolutionFiles (Topcoder tcDesc) root probID = do
    solutionFileExists <- liftIO $ isFile scaffoldPath
    unless solutionFileExists $ do
        templateSolution <- getTemplate "topcoder_solution_template.cs"
        liftIO $
            writeTextFile scaffoldPath $ T.append templateSolution $ T.unlines solutionDeclaration
  where
    problemDir = root </> fromText probID
    scaffoldPath = problemDir </> fromText (T.append probID ".cs")
    method = tcMethod tcDesc
    params = tcMethodParameters tcDesc
    declare val = T.concat [csharpType (tcValueType val) (tcValueDimension val), " ", tcValueName val]
    solutionDeclaration = [
        T.concat ["public class ", tcClassName tcDesc],
        "{",
        T.concat $ ["    public ", declare method, "("] ++
                    intersperse ", " (map declare params) ++
                    [")"],
        "    {",
        T.concat ["        ", declare (method {tcValueName = "result"}),
                  " = default(", csharpType (tcValueType method) (tcValueDimension method), ");"],
        "        return result;",
        "    }",
        "}"
        ]


inlineCSharpCode :: ProblemID -> CaideIO ()
inlineCSharpCode probID = do
    root <- caideRoot
    h <- readProblemConfig probID
    probType <- getProp h "problem" "type"
    let problemDir = root </> fromText probID
        solutionPath = problemDir </> fromText (T.append probID ".cs")
        mainProgramPath = problemDir </> "main.cs"
        inlinedCodePath = problemDir </> "submission.cs"

    case probType of
        Topcoder _ -> liftIO $ copyFile solutionPath inlinedCodePath
        Stream _ _ -> do
            mainCode <- readTextFile' mainProgramPath
            liftIO $ do
                copyFile solutionPath inlinedCodePath
                appendTextFile inlinedCodePath mainCode

    liftIO $ copyFile inlinedCodePath $ root </> "submission.cs"

generateTesterCode :: ProblemType -> [T.Text]
generateTesterCode (Topcoder tcDesc) = [
    "partial class CaideTester",
    "{",
    "    public const bool IS_TOPCODER_PROBLEM = true;",
    "",
    "    public static void TopcoderSolve(TextReader input, TextWriter output) {",
    "        ReadTopcoderInput(input);",
    T.append "        " $ generateSolutionCall tcDesc,
    "        WriteTopcoderOutput(output);",
    "    }",
    "",
    "    private static void ReadIfTopcoderProblem(TextReader input, TextReader output) {",
    "        ReadTopcoderInput(input);",
    "        ReadTopcoderOutput(output);",
    "    }",
    "    private static void ReadTopcoderInput(TextReader reader) {"
    ] ++ map (T.append "        ") (concatMap tcInit (tcMethodParameters tcDesc)) ++ [
    "    }",
    "    private static void ReadTopcoderOutput(TextReader reader) {"
    ] ++ [
    T.concat ["        result = ",
              tcDeserializer (tcValueType returnValue) (tcValueDimension returnValue),
              ".Deserialize(reader);"],
    "    }",
    "",
    "    private static void WriteTopcoderInput(TextWriter writer) {"
    ] ++ map (T.append "        " . tcWrite) (tcMethodParameters tcDesc) ++ [
    "    }",
    "",
    "    private static void WriteTopcoderOutput(TextWriter writer) {"
    ] ++ [T.append "        " $ tcWrite returnValue] ++ [
    "    }",
    ""
    ] ++ map (T.append "    " . tcDeclare) (tcMethodParameters tcDesc) ++ [
    T.append "    " $ tcDeclare returnValue,
    "}"
    ]
  where
    returnValue = (tcMethod tcDesc) {tcValueName = "result"}

generateTesterCode _ = [
    "partial class CaideTester",
    "{",
    "   public const bool IS_TOPCODER_PROBLEM = false;",
    "",
    "   public static void TopcoderSolve(TextReader input, TextWriter output) {",
    "   }",
    "",
    "   private static void ReadIfTopcoderProblem(TextReader input, TextReader output) {",
    "   }",
    "}"
    ]

generateSolutionCall :: TopcoderProblemDescriptor -> T.Text
generateSolutionCall tcDesc = T.concat $ [
    "result = new ", tcClassName tcDesc, "().", tcValueName method, "("
    ] ++ intersperse ", " (map tcValueName methodParams) ++ [
    ");"
    ]
  where
    method = tcMethod tcDesc
    methodParams = tcMethodParameters tcDesc

tcInit :: TopcoderValue -> [T.Text]
tcInit (TopcoderValue name type_ dimension) = [
    "Caide.TCSerializeUtil.SkipUpTo(reader, c => !char.IsWhiteSpace(c));",
    "Caide.TCSerializeUtil.SkipWhile(reader, char.IsWhiteSpace);",
    T.concat [name, " = ", tcDeserializer type_ dimension, ".Deserialize(reader);"]
    ]

tcWrite :: TopcoderValue -> T.Text
tcWrite (TopcoderValue name type_ dimension) = T.concat [
    tcDeserializer type_ dimension, ".Serialize(writer, ", name, ");"
    ]

tcDeserializer :: TopcoderType -> Int -> T.Text
tcDeserializer TCInt 0 = "new Caide.IntSerializer()"
tcDeserializer TCLong 0 = "new Caide.LongSerializer()"
tcDeserializer TCDouble 0 = "new Caide.DoubleSerializer()"
tcDeserializer TCString 0 = "new Caide.StringSerializer()"
tcDeserializer _ dimension | dimension <= 0  = "ERROR: Unknown type"
tcDeserializer type_ dimension = T.concat ["Caide.S.v(", tcDeserializer type_ (dimension-1), ")"]

tcDeclare :: TopcoderValue -> T.Text
tcDeclare (TopcoderValue name type_ dimension) = T.concat [
    "static ", csharpType type_ dimension, " ", name, ";"]

csharpType :: TopcoderType -> Int -> T.Text
csharpType TCInt 0 = "int"
csharpType TCLong 0 = "long"
csharpType TCDouble 0 = "double"
csharpType TCString 0 = "string"
csharpType _ dimension | dimension <= 0 = "ERROR: Unknown type"
csharpType type_ dimension = T.concat $ csharpType type_ 0 : replicate dimension "[]"

