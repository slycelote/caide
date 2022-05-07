{-# LANGUAGE OverloadedStrings #-}
module Caide.Commands.ParseContest(
      createContest
) where

import Data.List (find)

import Caide.Types (CaideIO, throw)

import Caide.Parsers.Common (URL, ContestParser(..))
import Caide.Parsers.CodeforcesContest
import Caide.Parsers.CodeChefContest
import qualified Caide.Parsers.LeetCodeContest as LeetCode

createContest :: URL -> CaideIO ()
createContest contestUrl = case findContestParser contestUrl of
    Nothing -> throw $ contestUrl <> " is not recognized as a supported contest URL"
    Just contestParser -> contestParser `parseContest` contestUrl

contestParsers :: [ContestParser]
contestParsers = [codeforcesContestParser, codeChefContestParser, LeetCode.contestParser]

findContestParser :: URL -> Maybe ContestParser
findContestParser url = find (`contestUrlMatches` url) contestParsers


