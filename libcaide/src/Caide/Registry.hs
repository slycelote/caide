module Caide.Registry (
      languages
    , findLanguage
) where

import Control.Applicative ((<$>))
import Data.List (find)

import Caide.Types
import qualified Caide.CPP.CPPSimple as CPPSimple (language)

languages :: [([String], ProgrammingLanguage)]
languages = [(["simplecpp", "simplec++"], CPPSimple.language)]

findLanguage :: String -> Maybe ProgrammingLanguage
findLanguage name = snd <$> find (\(names, _) -> name `elem` names) languages
