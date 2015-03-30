{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.Codeforces(
      codeforcesParser
) where

import Data.Array ((!))
import qualified Data.Text as T

import Network.URI (parseURI, uriAuthority, uriRegName)

import Text.HTML.TagSoup (fromTagText, innerText,
                          parseTags, sections, fromAttrib, Tag)
import Text.HTML.TagSoup.Utils

import Text.Regex.TDFA.Text (Regex)
import Text.Regex.Base.RegexLike (makeRegex, matchAllText)

import Caide.Parsers.Common (replaceBr)
import Caide.Types


codeforcesParser :: HtmlParser
codeforcesParser = HtmlParser
    { chelperId = "codeforces"
    , htmlParserUrlMatches = isCodeForcesUrl
    , parseFromHtml = doParse . parseTags
    }

isCodeForcesUrl :: URL -> Bool
isCodeForcesUrl url = case parseURI (T.unpack url) >>= uriAuthority of
    Nothing   -> False
    Just auth -> uriRegName auth `elem` ["codeforces.com", "www.codeforces.com", "codeforces.ru", "www.codeforces.ru"]

doParse :: [Tag T.Text] -> Either T.Text (Problem, [TestCase])
doParse tags =
    if null beforeTitleDiv
    then Left "Couldn't parse problem statement"
    else Right (Problem title probId probType, testCases)
  where
    statement = dropWhile (~~/== "<div class=problem-statement>") tags
    beforeTitleDiv = drop 1 . dropWhile (~~/== "<div class=title>") $ statement
    title = fromTagText $ head beforeTitleDiv
    inputDivs = sections (~~== "<div class=input>") statement
    outputDivs = sections (~~== "<div class=output>") statement

    extractText = innerText . replaceBr . takeWhile (~/= "</pre>") . dropWhile (~/= "<pre>")
    inputs = map extractText inputDivs
    outputs = map extractText outputDivs
    testCases = zipWith TestCase inputs outputs

    -- Contest
    sidebar = dropWhile (~/= "<div id=sidebar>") tags
    rtable = takeWhile (~/= "</table>") . dropWhile (~~/== "<table class=rtable>") $ sidebar
    anchors = sections (~== "<a>") rtable
    links = map (fromAttrib "href" . head) anchors
    allMatches = concatMap (matchAllText contestUrlRegex) links
    contestIds = map (fst . (!1)) allMatches

    probIdPrefix = if length contestIds == 1
                   then T.append "cf" (head contestIds)
                   else "cfproblem"
    probId = T.snoc probIdPrefix (T.head title)

    probType = Stream StdIn StdOut

contestUrlRegex :: Regex
contestUrlRegex = makeRegex (".*/([[:digit:]]+)$" :: String)

