{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Caide.Problem(
      Problem(..)
    , jsonEncodeProblem
    , readProblemInfo
) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))

import Filesystem.Util (pathToText)

import Caide.Configuration (readProblemConfig)
import Caide.Types (Problem(..), ProblemType(..), TopcoderProblemDescriptor(..), ProblemID,
    InputSource(..), OutputTarget(..), CaideIO, getProp)

readProblemInfo :: ProblemID -> CaideIO Problem
readProblemInfo probId = do
    hProblemInfo <- readProblemConfig probId
    pname <- getProp hProblemInfo "problem" "name"
    ptype <- getProp hProblemInfo "problem" "type"
    return $ Problem
        { problemName = pname
        , problemId = probId
        , problemType = ptype
        }

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

