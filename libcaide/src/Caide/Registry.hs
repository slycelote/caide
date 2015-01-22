{-# LANGUAGE CPP, OverloadedStrings #-}

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
import Data.Text (Text)
import qualified Data.Text as T

import Caide.Types
import qualified Caide.CPP.CPPSimple as CPPSimple
#ifdef CLANG_INLINER
import qualified Caide.CPP.CPP as CPP
#endif
import qualified Caide.CSharp.CSharpSimple as CSharpSimple
import qualified Caide.Builders.None as None
import qualified Caide.Builders.Custom as Custom

import qualified Caide.Features.Codelite as Codelite
import Caide.Parsers.Codeforces (codeforcesParser, codeforcesContestParser)
import Caide.Parsers.CodeChef (codeChefParser)


findLanguage :: Text -> Maybe ProgrammingLanguage
findLanguage name = snd <$> find (\(names, _) -> T.map toLower name `elem` names) languages

problemParsers :: [ProblemParser]
problemParsers = [codeforcesParser, codeChefParser]

findProblemParser :: URL -> Maybe ProblemParser
findProblemParser url = find (`problemUrlMatches` url) problemParsers

contestParsers :: [ContestParser]
contestParsers = [codeforcesContestParser]

findContestParser :: URL -> Maybe ContestParser
findContestParser url = find (`contestUrlMatches` url) contestParsers


builders :: [(Text, Builder)]
builders = [("none", None.builder)]

findBuilder :: T.Text -> Builder
findBuilder name = case find ((== T.map toLower name) . fst) builders of
    Just (_, builder) -> builder
    Nothing           -> Custom.builder name

features :: [(Text, Feature)]
features = [ ("codelite", Codelite.feature)
           ]

findFeature :: Text -> Maybe Feature
findFeature name = snd <$> find ((== T.map toLower name). fst) features

languages :: [([Text], ProgrammingLanguage)]
languages = [ (["simplecpp", "simplec++"], CPPSimple.language)
#ifdef CLANG_INLINER
            , (["cpp", "c++"], CPP.language)
#endif
            , (["c#", "csharp"], CSharpSimple.language)
            ]

