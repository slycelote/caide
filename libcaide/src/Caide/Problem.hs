{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Caide.Problem(
      Problem(..)
    , ProblemState(..)
    , jsonEncodeProblem
    , readProblemInfo
    , readProblemState
) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Data.Text (Text)

import Filesystem.Util (pathToText)

import qualified Caide.Configuration as Conf
import Caide.Types (Problem(..), ProblemType(..), TopcoderProblemDescriptor(..), ProblemID,
    InputSource(..), OutputTarget(..), CaideIO, getProp)

readProblemInfo :: ProblemID -> CaideIO Problem
readProblemInfo probId = do
    hProblemInfo <- Conf.readProblemConfig probId
    pname <- getProp hProblemInfo "problem" "name"
    ptype <- getProp hProblemInfo "problem" "type"
    return $ Problem
        { problemName = pname
        , problemId = probId
        , problemType = ptype
        }

data ProblemState = ProblemState
                  { currentLanguage :: Text
                  }

readProblemState :: ProblemID -> CaideIO ProblemState
readProblemState probId = do
    hProblemState <- Conf.readProblemState probId
    currentLanguage <- getProp hProblemState "problem" "language"
    return $ ProblemState{..}

jsonEncodeProblem :: Problem -> Aeson.Value
jsonEncodeProblem Problem{..} = Aeson.object $
    [ "id" .= problemId
    , "name" .= problemName
    ] ++ typeEntries problemType
  where
    typeEntries (Topcoder topcoderDesc) = ["topcoder" .= encodeTopcoderDesc topcoderDesc]
    typeEntries (Stream input output) = ["input" .= encodeInput input, "output" .= encodeOutput output]

encodeTopcoderDesc :: TopcoderProblemDescriptor -> Aeson.Value
encodeTopcoderDesc _ = Aeson.object [] -- TODO

encodeInput :: InputSource -> Aeson.Value
encodeInput (FileInput path) = Aeson.object ["file" .= pathToText path]
encodeInput _ = Aeson.object []

encodeOutput :: OutputTarget -> Aeson.Value
encodeOutput (FileOutput path) = Aeson.object ["file" .= pathToText path]
encodeOutput _ = Aeson.object []

