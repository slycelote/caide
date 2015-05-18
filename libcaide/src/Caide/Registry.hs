{-# LANGUAGE CPP, OverloadedStrings #-}

module Caide.Registry(
      languages
    , findLanguage
    , findFeature
    , findHtmlParser
    , findProblemParser
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

import qualified Caide.Features.Codelite as Codelite

import Caide.Parsers.Common (htmlParserToProblemParser)
import Caide.Parsers.Codeforces
import Caide.Parsers.CodeChef
import Caide.Parsers.GCJ
import Caide.Parsers.HackerRank
import Caide.Parsers.POJ
import Caide.Parsers.RCC
import Caide.Parsers.Timus
import Caide.Parsers.Yandex



htmlParsers :: [HtmlParser]
htmlParsers = [codeforcesParser, codeChefParser, timusParser, gcjParser, pojParser, rccParser, hackerRankParser, yandexParser]

problemParsers :: [ProblemParser]
problemParsers = map htmlParserToProblemParser [codeforcesParser, codeChefParser, timusParser,
    pojParser, rccParser]

findHtmlParser :: Text -> Maybe HtmlParser
findHtmlParser chid = find ((== chid) . chelperId) htmlParsers

findProblemParser :: URL -> Maybe ProblemParser
findProblemParser url = find (`problemUrlMatches` url) problemParsers

features :: [(Text, Feature)]
features = [ ("codelite", Codelite.feature)
           ]

findFeature :: Text -> Maybe Feature
findFeature name = snd <$> find ((== T.map toLower name). fst) features

languages :: [([Text], ProgrammingLanguage)]
languages = [ (["simplec++", "simplecpp"], CPPSimple.language)
#ifdef CLANG_INLINER
            , (["c++", "cpp"], CPP.language)
#endif
            , (["c#", "csharp", "cs"], CSharpSimple.language)
            ]

findLanguage :: Text -> Maybe ([Text], ProgrammingLanguage)
findLanguage name = find (\(names, _) -> T.map toLower name `elem` names) languages

