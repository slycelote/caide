{-# LANGUAGE CPP, OverloadedStrings #-}

module Caide.Registry(
      languages
    , findLanguage
    , findFeature
    , findCHelperProblemParser
    , findCHelperProblemParserByURL
    , findProblemParser
) where

import Data.Char (toLower)
import Data.Function ((&))
import Data.List (find)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Caide.Monad (Feature)
import Caide.Settings (Settings)
import Caide.Types.ProgrammingLanguage (ProgrammingLanguage)

import qualified Caide.CPP.CPPSimple as CPPSimple
#ifdef CLANG_INLINER
import qualified Caide.CPP.CPP as CPP
#endif
import qualified Caide.CSharp.CSharpSimple as CSharpSimple

import qualified Caide.Features.Codelite as Codelite

import Caide.Parsers.Common (URL, CHelperProblemParser(CHelperProblemParser, chelperId, chelperUrlMatches),
    ProblemParser(problemUrlMatches), makeProblemParser)

import qualified Caide.Parsers.Codeforces as Codeforces
import qualified Caide.Parsers.CodeChef as CodeChef
import qualified Caide.Parsers.GCJ as GCJ
import qualified Caide.Parsers.HackerRank as HackerRank
import qualified Caide.Parsers.LeetCode as LeetCode
import qualified Caide.Parsers.POJ as POJ
import qualified Caide.Parsers.Timus as Timus
import qualified Caide.Parsers.Yandex as Yandex
import qualified Caide.GenericLanguage as GenericLanguage


chelperParsers :: [CHelperProblemParser]
chelperParsers = [ CHelperProblemParser Codeforces.chelperId Codeforces.isSupportedUrl Codeforces.htmlParser
                 , CodeChef.chelperProblemParser
                 , CHelperProblemParser GCJ.chelperId GCJ.isSupportedUrl GCJ.htmlParser
                 , CHelperProblemParser HackerRank.chelperId HackerRank.isSupportedUrl HackerRank.htmlParser
                 , CHelperProblemParser POJ.chelperId POJ.isSupportedUrl POJ.htmlParser
                 , CHelperProblemParser Timus.chelperId Timus.isSupportedUrl Timus.htmlParser
                 , CHelperProblemParser Yandex.chelperId Yandex.isSupportedUrl Yandex.htmlParser
                 ]

problemParsers :: [ProblemParser]
problemParsers = [ makeProblemParser Codeforces.isSupportedUrl Codeforces.htmlParser
                 , CodeChef.problemParser
                 , makeProblemParser HackerRank.isSupportedUrl HackerRank.htmlParser
                 , LeetCode.problemParser
                 , makeProblemParser POJ.isSupportedUrl POJ.htmlParser
                 , makeProblemParser Timus.isSupportedUrl Timus.htmlParser
                 ]

findCHelperProblemParser :: Text -> Maybe CHelperProblemParser
findCHelperProblemParser chid = find ((== chid) . chelperId) chelperParsers

findCHelperProblemParserByURL :: URL -> Maybe CHelperProblemParser
findCHelperProblemParserByURL url = find (`chelperUrlMatches` url) chelperParsers

findProblemParser :: URL -> Maybe ProblemParser
findProblemParser url = find (`problemUrlMatches` url) problemParsers

features :: [(Text, Feature)]
features = [ ("codelite", Codelite.feature)
           ]

findFeature :: Text -> Maybe Feature
findFeature name = snd <$> find ((== T.map toLower name). fst) features

languages :: NE.NonEmpty (NE.NonEmpty Text, ProgrammingLanguage)
languages = NE.fromList
          [ (NE.fromList ["simplec++", "simplecpp"], CPPSimple.language)
          , (NE.fromList ["c++", "cpp"], cppLanguage)
          , (NE.fromList ["c#", "csharp", "cs"], CSharpSimple.language)
          ]
  where
#ifdef CLANG_INLINER
    cppLanguage = CPP.language
#else
    cppLanguage = CPPSimple.language
#endif

findLanguage :: Settings -> Text -> (NE.NonEmpty Text, ProgrammingLanguage)
findLanguage _settings name =
    find (\(names, _) -> T.map toLower name `elem` names) languages
    & fromMaybe (NE.fromList [name], GenericLanguage.makeLanguage name)

