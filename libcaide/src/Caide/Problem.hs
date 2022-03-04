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
import Data.Maybe (fromMaybe, listToMaybe)
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
    typeEntries (Topcoder topcoderDesc) =
        [ "type" .= T.pack "topcoder"
        , "solutionClass" .= encodeTopcoderDesc topcoderDesc problemCodeSnippets True
        ]
    typeEntries (LeetCodeMethod method) =
        [ "type" .= T.pack "leetcode"
        , "solutionClass" .= encodeTopcoderDesc
            (TopcoderProblemDescription defaultLeetCodeClassName method) problemCodeSnippets False
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
      VariableName  | lang `elem` ["erlang"] -> pascalCase
      _k            | lang `elem` ["erlang"] -> snakeCase
                    | lang `elem` ["racket"] -> hyphenSeparated
      ClassName     -> origIdentifier
      _k            | lang `elem` ["ruby", "rust", "elixir"] -> snakeCase
                    | otherwise -> origIdentifier

    candidates = bestGuess : [ hyphenSeparated, snakeCase, pascalCase ]
  in candidates & filter isIdentOk & listToMaybe & fromMaybe origIdentifier

encodeValue :: Map.Map Text Text -> IdentifierKind -> TopcoderValue -> Aeson.Value
encodeValue codeSnippets identKind TopcoderValue{..} = Aeson.object $
    [ "name" .= tcValueName
    , "identifier" .= Aeson.object [ lang .= findIdent lang | lang <- languages ]
    , "dimension" .= tcValueDimension
    , "type" .= Aeson.String (encodeType tcValueType)
    ]
  where
    languages = Map.keys codeSnippets ++
        [ "cpp", "java", "python", "python3", "c", "csharp", "javascript", "ruby", "swift", "golang", "scala", "kotlin", "rust", "php", "typescript", "racket", "erlang", "elixir" ]
    findIdent lang = findIdentifier tcValueName lang identKind (Map.lookup lang codeSnippets)

encodeMethod :: Map.Map Text Text -> TopcoderMethod -> Bool -> Aeson.Value
encodeMethod codeSnippets TopcoderMethod{tcMethod, tcParameters} isTopcoder = Aeson.object $
    [ "method" .= encodeValue codeSnippets (if isTopcoder then KeepAsWritten else MethodName) tcMethod
    , "parameters" .= Aeson.Array (Vector.fromList (
            map (encodeValue codeSnippets (if isTopcoder then KeepAsWritten else VariableName)) tcParameters))
    ]

encodeTopcoderDesc :: TopcoderProblemDescription -> Map.Map Text Text -> Bool -> Aeson.Value
encodeTopcoderDesc TopcoderProblemDescription{tcClassName, tcSingleMethod} codeSnippets isTopcoder = Aeson.object
    [ "className" .= tcClassName
    , "identifier" .= Aeson.object [ lang .= findIdent lang | lang <- languages ]
    , "methods" .= Aeson.Array (Vector.singleton $ encodeMethod codeSnippets tcSingleMethod isTopcoder)
    ]
  where
    languages = Map.keys codeSnippets ++
        [ "cpp", "java", "python", "python3", "c", "csharp", "javascript", "ruby", "swift", "golang", "scala", "kotlin", "rust", "php", "typescript", "racket", "erlang", "elixir" ]
    findIdent lang = findIdentifier tcClassName lang ClassName (Map.lookup lang codeSnippets)

encodeInput :: InputSource -> Aeson.Value
encodeInput (FileInput path) = Aeson.object ["file" .= pathToText path]
encodeInput (InputFilePattern p) = Aeson.object ["regex" .= p]
encodeInput StdIn = Aeson.object ["stdin" .= Aeson.Bool True]

encodeOutput :: OutputTarget -> Aeson.Value
encodeOutput (FileOutput path) = Aeson.object ["file" .= pathToText path]
encodeOutput StdOut = Aeson.object ["stdout" .= Aeson.Bool True]

