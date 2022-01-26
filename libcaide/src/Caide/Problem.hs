{-# LANGUAGE NamedFieldPuns, OverloadedStrings, RecordWildCards #-}
module Caide.Problem(
      Problem(..)
    , ProblemState(..)
    , jsonEncodeProblem
    , readProblemInfo
    , readProblemState
) where

import qualified Data.ByteString.Lazy as LBS
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
    typeEntries (Topcoder topcoderDesc) = ["type" .= T.pack "topcoder", "topcoder" .= encodeTopcoderDesc topcoderDesc]
    typeEntries (LeetCodeMethod method) = ["type" .= T.pack "leetcode", "topcoder" .= encodeTopcoderDesc (
        TopcoderProblemDescription defaultLeetCodeClassName method)]
    typeEntries (Stream input output) = ["type" .= T.pack "stream", "input" .= encodeInput input, "output" .= encodeOutput output]
    cleanLanguage "c++" = "cpp"
    cleanLanguage "c#" = "csharp"
    cleanLanguage s = s

encodeType :: TopcoderType -> Text
encodeType TCInt = "int"
encodeType TCLong = "long"
encodeType TCDouble = "double"
encodeType TCString = "string"
encodeType TCBool = "bool"

encodeValue :: TopcoderValue -> Aeson.Value
encodeValue TopcoderValue{..} = Aeson.object $
    [ "name" .= tcValueName
    , "dimension" .= tcValueDimension
    , "type" .= Aeson.String (encodeType tcValueType)
    ]

encodeMethod :: TopcoderMethod -> Aeson.Value
encodeMethod TopcoderMethod{tcMethod, tcParameters} = Aeson.object $
    [ "method" .= encodeValue tcMethod
    , "parameters" .= Aeson.Array (Vector.fromList [ encodeValue v | v <- tcParameters ])
    ]

encodeTopcoderDesc :: TopcoderProblemDescription -> Aeson.Value
encodeTopcoderDesc desc = Aeson.object
    [ "className" .= tcClassName desc
    , "singleMethod" .= encodeMethod (tcSingleMethod desc)
    ]

encodeInput :: InputSource -> Aeson.Value
encodeInput (FileInput path) = Aeson.object ["file" .= pathToText path]
encodeInput (InputFilePattern p) = Aeson.object ["regex" .= p]
encodeInput StdIn = Aeson.object ["stdin" .= Aeson.Bool True]

encodeOutput :: OutputTarget -> Aeson.Value
encodeOutput (FileOutput path) = Aeson.object ["file" .= pathToText path]
encodeOutput StdOut = Aeson.object ["stdout" .= Aeson.Bool True]

