{-# LANGUAGE NamedFieldPuns, OverloadedStrings, RecordWildCards #-}
module Caide.Problem(
      Problem(..)
    , ProblemState(..)
    , jsonEncodeProblem
    , readProblemInfo
    , readProblemState
) where

import qualified Data.ByteString.Lazy as LBS
import Data.Char (isUpper, toLower, toUpper)
import Data.Function ((&))
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector

import Filesystem.Util (pathToText)

import qualified Caide.Configuration as Conf
import Caide.Types

readProblemInfo :: ProblemID -> CaideIO Problem
readProblemInfo probId = do
    hProblemInfo <- Conf.readProblemConfig probId
    pname <- getProp hProblemInfo "problem" "name"
    ptype <- getProp hProblemInfo "problem" "type"
    fpTolerance <- getProp hProblemInfo "problem" "double_precision"

    -- We keep snippets in the state file to avoid huge chunk of text in problem.ini.
    -- Users won't need to modify per-problem snippets anyway (if they do they'll modify
    -- generated code).
    hProblemState <- Conf.readProblemState probId
    snippets <- (LBS.fromStrict . T.encodeUtf8) <$>
        getProp hProblemState "problem" "snippets" `Conf.orDefault` "{}"
    return $ Problem
        { problemName = pname
        , problemId = probId
        , problemFloatTolerance = fpTolerance
        , problemType = ptype
        , problemCodeSnippets = fromMaybe Map.empty (Aeson.decode snippets)
        }

data ProblemState = ProblemState
                  { currentLanguage :: !Text
                  }

readProblemState :: ProblemID -> CaideIO ProblemState
readProblemState probId = do
    hProblemState <- Conf.readProblemState probId
    currentLanguage <- getProp hProblemState "problem" "language"
    return $ ProblemState{..}

jsonEncodeProblem :: Problem -> ProblemState -> Aeson.Value
jsonEncodeProblem Problem{..} ProblemState{..} = Aeson.object $
    [ "id" .= problemId
    , "name" .= problemName
    , "language" .= cleanLanguage currentLanguage
    ]
    ++ ["codeSnippets" .= problemCodeSnippets | not (Map.null problemCodeSnippets) ]
    ++ typeEntries problemType
  where
    typeEntries (Topcoder TopcoderProblemDescription{tcClassName, tcSingleMethod}) =
        [ "type" .= T.pack "topcoder"
        , "solutionClass" .=
              encodeSolutionClass tcClassName Nothing [tcSingleMethod] problemCodeSnippets True
        ]
    typeEntries (LeetCodeMethod method) =
        [ "type" .= T.pack "leetcode"
        , "solutionClass" .=
              encodeSolutionClass defaultLeetCodeClassName Nothing [method] problemCodeSnippets False
        ]
    typeEntries (LeetCodeClass className ctorParams methods) =
        [ "type" .= T.pack "leetcode"
        -- "system design" problem
        , "isMultiMethod" .= Aeson.Bool True
        , "solutionClass" .=
              encodeSolutionClass className (Just ctorParams) methods problemCodeSnippets False
        ]
    typeEntries (Stream input output) =
        [ "type" .= T.pack "stream"
        , "input" .= encodeInput input
        , "output" .= encodeOutput output
        ]
    cleanLanguage "c++" = "cpp"
    cleanLanguage "c#" = "csharp"
    cleanLanguage s = s

encodeType :: TopcoderType -> Text
encodeType TCInt = "int"
encodeType TCLong = "long"
encodeType TCDouble = "double"
encodeType TCString = "string"
encodeType TCBool = "bool"
encodeType TCVoid = "void"
encodeType (TypeName s) = s

-- TODO: register snake-case/hyphen-separated/etc as functions in Jinja template?
data IdentifierKind = VariableName | MethodName | ClassName | KeepAsWritten

-- LeetCode uses different name convention for different languages.
findIdentifier :: Text -> Text -> IdentifierKind -> Maybe Text -> Text
findIdentifier origIdentifier lang identKind mbCode =
  let
    hyphenSeparated = T.concatMap
        (\c -> (if isUpper c then "-" else "") <> T.singleton (toLower c))
        origIdentifier
    snakeCase = T.replace "-" "_" hyphenSeparated
    pascalCase = T.singleton (toUpper $ T.head origIdentifier) <> T.tail origIdentifier

    isIdentOk ident = case mbCode of
        Nothing -> True
        Just code -> ident `T.isInfixOf` code

    bestGuess = case identKind of
      KeepAsWritten -> origIdentifier
      MethodName    | lang `elem` ["c", "csharp", "go"] -> pascalCase
      VariableName  | lang == "erlang" -> pascalCase
      _k            | lang == "erlang" -> snakeCase
                    | lang == "racket" -> hyphenSeparated
      ClassName     -> origIdentifier
      _k            | lang `elem` ["ruby", "rust", "elixir"] -> snakeCase
                    | otherwise -> origIdentifier

    candidates = bestGuess : [ hyphenSeparated, snakeCase, pascalCase ]
  in candidates & List.find isIdentOk & fromMaybe origIdentifier

encodeSolutionClass :: Text -> Maybe [TopcoderValue] -> [TopcoderMethod] -> Map.Map Text Text -> Bool -> Aeson.Value
encodeSolutionClass className mbConstructorParams methods codeSnippets isTopcoder = Aeson.object $
    [ "className" .= className
    , "identifier" .= Aeson.object [ lang .= findClassNameIdentifier lang | lang <- languages ]
    , "methods" .= Aeson.Array (Vector.fromList (map encodeMethod methods))
    ]
    ++ [ "constructor" .= Aeson.object [ "parameters" .= encodeParameters ctorParams ] | Just ctorParams <- [mbConstructorParams] ]
  where
    languages = Map.keys codeSnippets ++
        [ "cpp", "java", "python", "python3", "c", "csharp", "javascript", "ruby", "swift", "golang", "scala", "kotlin", "rust", "php", "typescript", "racket", "erlang", "elixir" ]
    findClassNameIdentifier lang = if isTopcoder
                                     then lang
                                     else findIdentifier className lang ClassName (Map.lookup lang codeSnippets)

    encodeParameters params = Aeson.Array (Vector.fromList (
            map (encodeValue (if isTopcoder then KeepAsWritten else VariableName)) params))

    encodeValue :: IdentifierKind -> TopcoderValue -> Aeson.Value
    encodeValue identKind TopcoderValue{..} = Aeson.object
        [ "name" .= tcValueName
        , "identifier" .= Aeson.object [ lang .= findIdent lang | lang <- languages ]
        , "dimension" .= tcValueDimension
        , "type" .= Aeson.String (encodeType tcValueType)
        ]
      where
        findIdent lang = findIdentifier tcValueName lang identKind (Map.lookup lang codeSnippets)

    encodeMethod :: TopcoderMethod -> Aeson.Value
    encodeMethod TopcoderMethod{tcMethod, tcParameters} = Aeson.object
        [ "method" .= encodeValue (if isTopcoder then KeepAsWritten else MethodName) tcMethod
        , "parameters" .= encodeParameters tcParameters
        ]


encodeInput :: InputSource -> Aeson.Value
encodeInput (FileInput path) = Aeson.object ["file" .= pathToText path]
encodeInput (InputFilePattern p) = Aeson.object ["regex" .= p]
encodeInput StdIn = Aeson.object ["stdin" .= Aeson.Bool True]

encodeOutput :: OutputTarget -> Aeson.Value
encodeOutput (FileOutput path) = Aeson.object ["file" .= pathToText path]
encodeOutput StdOut = Aeson.object ["stdout" .= Aeson.Bool True]

