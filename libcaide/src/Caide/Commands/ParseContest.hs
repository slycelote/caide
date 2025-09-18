{-# LANGUAGE OverloadedStrings #-}
module Caide.Commands.ParseContest(
      createContest
) where

import Control.Monad.Extended (liftIO)
import Data.List (find)

import Caide.Monad (CaideIO, throw, caideHttpClient)

import Caide.Commands.ParseProblem (parseProblems)
import Caide.Parsers.Common (URL, ContestParser(..), ContestParserResult(Urls, Problems))
import Caide.Parsers.CodeforcesContest
import Caide.Parsers.CodeChefContest
import qualified Caide.Parsers.LeetCodeContest as LeetCode

createContest :: URL -> CaideIO ()
createContest contestUrl = case findContestParser contestUrl of
    Nothing -> throw $ contestUrl <> " is not recognized as a supported contest URL"
    Just contestParser -> do
        client <- caideHttpClient
        result <- liftIO $ parseContest contestParser client contestUrl
        case result of
            Left err -> throw err
            Right (Urls urls) -> parseProblems 3 urls
            Right (Problems _problems) -> throw "TODO"

contestParsers :: [ContestParser]
contestParsers = [codeforcesContestParser, codeChefContestParser, LeetCode.contestParser]

findContestParser :: URL -> Maybe ContestParser
findContestParser url = find (`contestUrlMatches` url) contestParsers


