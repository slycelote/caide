{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.Codeforces(
      codeforcesParser
    , codeforcesContestParser
) where

import Control.Applicative ((<$>))
import Data.Array ((!))
import Data.Maybe (mapMaybe)
import qualified Data.Text as T

import Network.URI (parseURI, uriAuthority, uriRegName)

import Text.HTML.TagSoup (fromTagText, innerText, partitions,
                          parseTags, sections, fromAttrib, Tag)
import Text.HTML.TagSoup.Utils

import Text.Regex.TDFA.Text (Regex)
import Text.Regex.Base.RegexLike (makeRegex, matchAllText)

import Caide.Parsers.Common (replaceBr)
import Caide.Types
import Caide.Util (downloadDocument)


codeforcesParser :: HtmlParser
codeforcesParser = HtmlParser
    { chelperId = "codeforces"
    , htmlParserUrlMatches = isCodeForcesUrl
    , parseFromHtml = doParse . parseTags
    }

codeforcesContestParser :: ContestParser
codeforcesContestParser = ContestParser
    { contestUrlMatches = isCodeForcesUrl
    , parseContest = doParseContest
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

doParseContest :: URL -> IO (Either T.Text [URL])
doParseContest url = parseCfContest <$> downloadDocument url

parseCfContest :: Either T.Text URL -> Either T.Text [T.Text]
parseCfContest (Left err)   = Left err
parseCfContest (Right cont) = if null problemsTable
                              then Left "Couldn't parse contest"
                              else Right problems
  where
    tags = parseTags cont
    content = dropWhile (~/= "<div id=content>") tags
    problemsTable = takeWhile (~/= "</table>") . dropWhile (~~/== "<table class=problems>") $ content
    trs = partitions (~== "<tr>") problemsTable
    problems = mapMaybe extractURL trs

extractURL :: [Tag T.Text] -> Maybe T.Text
extractURL tr = T.append (T.pack "http://codeforces.com") <$> if null anchors then Nothing else Just url
  where
    td = dropWhile (~~/== "<td class=id>") tr
    anchors = dropWhile (~/= "<a>") td
    anchor = head anchors
    url = fromAttrib (T.pack "href") anchor

