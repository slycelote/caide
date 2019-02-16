{-# LANGUAGE CPP, OverloadedStrings #-}
module Caide.Parsers.HackerRank(
      hackerRankParser
) where

#ifndef AMP
import Control.Applicative ((<$>))
#endif
import Control.Applicative ((<|>))
import Data.Char (isAlphaNum, isSpace)
import Data.List (find)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import Network.URI (parseURI, uriAuthority, uriRegName)

import Text.HTML.TagSoup (fromAttrib, maybeTagText, parseTags, partitions,
    Tag(TagText))
import Text.HTML.TagSoup.Utils

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

    metaTitle = do
        meta <- find (~== "<meta property=og:title") tags
        return $ fromAttrib "content" meta

    pageTitle = do
        titleText <- listToMaybe $ drop 1 . dropWhile (~/= "<title>") $ tags
        maybeTagText titleText

    h2Title = do
        h2Text <- listToMaybe $ drop 1 . dropWhile (~~/== "<h2 class=hr_tour-challenge-name>") $ tags
        maybeTagText h2Text

    rawTitle = fromMaybe "Unknown" $ pageTitle <|> metaTitle <|> h2Title
    title = T.strip . T.takeWhile (/= '|') $ rawTitle

    probId = T.append "hr" . T.filter isAlphaNum $ title

    samples = partitions (\tag -> tag ~~== "<div class=challenge_sample_input_body>" || tag ~~== "<div class=challenge_sample_output_body>") tags

    pres = map (drop 1 . takeWhile (~/= "</pre>") . dropWhile (~/= "<pre>") ) samples
    texts = map extractText pres
    t = drop (length texts `mod` 2) texts
    testCases = [TestCase (t!!i) (t!!(i+1)) | i <- [0, 2 .. length t-2]]

    probType = Stream StdIn StdOut
    problem = (Problem title probId probType, testCases)

extractText :: [Tag T.Text] -> T.Text
extractText tags = T.unlines [t | TagText t <- tags, not (T.all isSpace t)]

