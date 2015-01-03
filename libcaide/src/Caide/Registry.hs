{-# LANGUAGE CPP #-}

module Caide.Registry (
      languages
    , findLanguage
    , builders
    , findBuilder
    , findFeature
    , findProblemParser
    , findContestParser
) where

import Control.Applicative ((<$>))
import Data.Char (toLower)
import Data.List (find)

import Caide.Types
import qualified Caide.CPP.CPPSimple as CPPSimple
#ifdef CLANG_INLINER
import qualified Caide.CPP.CPP as CPP
#endif
import qualified Caide.Builders.None as None
import qualified Caide.Builders.Custom as Custom

import qualified Caide.Features.Codelite as Codelite
import qualified Caide.Features.VisualStudio as VS
import Caide.Parsers.Codeforces (codeforcesParser, codeforcesContestParser)
import Caide.Parsers.CodeChef (codeChefParser)


findLanguage :: String -> Maybe ProgrammingLanguage
findLanguage name = snd <$> find (\(names, _) -> map toLower name `elem` names) languages

problemParsers :: [ProblemParser]
problemParsers = [codeforcesParser, codeChefParser]

findProblemParser :: URL -> Maybe ProblemParser
findProblemParser url = find (`problemUrlMatches` url) problemParsers

contestParsers :: [ContestParser]
contestParsers = [codeforcesContestParser]

findContestParser :: URL -> Maybe ContestParser
findContestParser url = find (`contestUrlMatches` url) contestParsers


builders :: [(String, Builder)]
builders = [("none", None.builder)]

findBuilder :: String -> Builder
findBuilder name = case find ((== map toLower name) . fst) builders of
    Just (_, builder) -> builder
    Nothing           -> Custom.builder name

features :: [(String, Feature)]
features = [ ("codelite", Codelite.feature)
           , ("vs", VS.feature)
           ]

findFeature :: String -> Maybe Feature
findFeature name = snd <$> find ((== map toLower name). fst) features

languages :: [([String], ProgrammingLanguage)]
languages = [ (["simplecpp", "simplec++"], CPPSimple.language)
#ifdef CLANG_INLINER
            , (["cpp", "c++"], CPP.language)
#endif
            ]

