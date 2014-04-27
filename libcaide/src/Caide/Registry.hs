module Caide.Registry (
      languages
    , findLanguage
    , builders
    , findBuilder
) where

import Control.Applicative ((<$>))
import Data.List (find)

import Caide.Types
import qualified Caide.CPP.CPPSimple as CPPSimple
import qualified Caide.Builders.None as None
import qualified Caide.Builders.Custom as Custom

languages :: [([String], ProgrammingLanguage)]
languages = [(["simplecpp", "simplec++"], CPPSimple.language)]

findLanguage :: String -> Maybe ProgrammingLanguage
findLanguage name = snd <$> find (\(names, _) -> name `elem` names) languages

builders :: [(String, Builder)]
builders = [("None", None.builder)]

findBuilder :: String -> Builder
findBuilder name = case find ((== name) . fst) builders of
    Just (_, builder) -> builder
    Nothing           -> Custom.builder name
