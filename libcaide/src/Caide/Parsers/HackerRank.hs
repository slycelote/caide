{-# LANGUAGE CPP, OverloadedStrings #-}
module Caide.Parsers.HackerRank(
      hackerRankParser
) where

#ifndef AMP
import Control.Applicative ((<$>))
#endif
import Data.Char (isAlphaNum)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as T
import Network.URI (parseURI, uriAuthority, uriRegName)

import Text.HTML.TagSoup (maybeTagText, parseTags, partitions)
import Text.HTML.TagSoup.Utils

import Caide.Parsers.Common (normalizeText)

import Caide.Types

hackerRankParser :: HtmlParser
hackerRankParser = HtmlParser
    { chelperId = "hackerrank"
    , htmlParserUrlMatches = isHackerRankUrl
    , parseFromHtml = doParse
    }

isHackerRankUrl :: URL -> Bool
isHackerRankUrl url = case parseURI (T.unpack url) >>= uriAuthority of
    Nothing   -> False
    Just auth -> uriRegName auth `elem` ["hackerrank.com", "www.hackerrank.com"]


doParse :: T.Text -> Either T.Text (Problem, [TestCase])
doParse cont =
    if null testCases
    then Left "Couldn't parse problem"
    else Right problem
  where
    tags = parseTags cont

    h2Title = drop 1 . dropWhile (~~/== "<h2 class=hr_tour-challenge-name>") $ tags
    title' = T.strip <$> (listToMaybe h2Title >>= maybeTagText)
    probId' = T.filter isAlphaNum <$> title'
    title  = fromMaybe "Unknown" title'
    probId = T.append "hr" . fromMaybe "Unknown" $ probId'

    problemStatement = takeWhile (~~/== "<footer>") . dropWhile (~~/== "<div class=challenge-text>") $ tags

    pres = partitions (~~== "<pre>") problemStatement
    codes = mapMaybe (listToMaybe . drop 1 . dropWhile (~~/== "<code>")) pres
    texts = map normalizeText . mapMaybe maybeTagText $ codes
    t = drop (length texts `mod` 2) texts
    testCases = [TestCase (t!!i) (t!!(i+1)) | i <- [0, 2 .. length t-2]]

    probType = Stream StdIn StdOut
    problem = (Problem title probId probType, testCases)

