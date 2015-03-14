{-# LANGUAGE CPP, OverloadedStrings #-}

module Caide.Registry(
      languages
    , findLanguage
    , findFeature
    , findHtmlParser
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

import qualified Caide.Features.Codelite as Codelite
import Caide.Parsers.Codeforces
import Caide.Parsers.CodeChef
import Caide.Parsers.GCJ
import Caide.Parsers.Timus

import Caide.Util (runHtmlParser)



htmlParserToProblemParser :: HtmlParser -> ProblemParser
htmlParserToProblemParser htmlParser = ProblemParser
    { problemUrlMatches = htmlParserUrlMatches htmlParser
    , parseProblem = runHtmlParser (parseFromHtml htmlParser)
    }

htmlParsers :: [HtmlParser]
htmlParsers = [codeforcesParser, codeChefParser, timusParser, gcjParser]

problemParsers :: [ProblemParser]
problemParsers = map htmlParserToProblemParser [codeforcesParser, codeChefParser, timusParser]

findHtmlParser :: Text -> Maybe HtmlParser
findHtmlParser chid = find ((== chid) . chelperId) htmlParsers

findProblemParser :: URL -> Maybe ProblemParser
findProblemParser url = find (`problemUrlMatches` url) problemParsers

contestParsers :: [ContestParser]
contestParsers = [codeforcesContestParser, codeChefContestParser]

findContestParser :: URL -> Maybe ContestParser
findContestParser url = find (`contestUrlMatches` url) contestParsers

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

