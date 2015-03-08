{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.CodeChef(
      codeChefParser
    , codeChefContestParser
) where

import Control.Applicative ((<$>))
import Data.Maybe (mapMaybe)
import qualified Data.Text as T

import Network.URI (parseURI, uriAuthority, uriRegName)

import Text.HTML.TagSoup (Tag(..), innerText, fromAttrib, fromTagText, parseTags,
                         isTagCloseName, isTagOpenName, sections)

import Caide.Types
import Caide.Util (downloadDocument)
import Text.HTML.TagSoup.Utils ((~==), (~/=), (~~/==), (~~==), isTagName, mergeTextTags)


codeChefParser :: HtmlParser
codeChefParser = HtmlParser
    { chelperId = "codechef"
    , htmlParserUrlMatches = isCodeChefUrl
    , parseFromHtml = doParse
    }

codeChefContestParser :: ContestParser
codeChefContestParser = ContestParser
    { contestUrlMatches = isCodeChefUrl
    , parseContest = doParseContest
    }

isCodeChefUrl :: URL -> Bool
isCodeChefUrl url = case parseURI (T.unpack url) >>= uriAuthority of
    Nothing   -> False
    Just auth -> uriRegName auth `elem` ["codechef.com", "www.codechef.com"]

doParse :: T.Text -> Either T.Text (Problem, [TestCase])
doParse cont =
    if null pres || null problemCode
    then Left "Couldn't parse problem statement"
    else Right (Problem title probId probType, testCases)
  where
    tags = parseTags cont

    -- problem ID
    problemCode = dropWhile (~/= "<span id=problem-code>") tags
    spanContents = takeWhile (~/= "</span>") problemCode
    -- TODO title
    title = T.strip $ innerText spanContents
    probId = T.append "chef" title

    -- test cases
    problemPage = dropWhile (~/= "<div id=problem-page>") tags
    content = takeWhile (~/= "</div>") . dropWhile (~~/== "<div class=content>") $ problemPage
    pres = sections (~== "<pre>") content
    testsContainer = drop 1 . takeWhile (~/= "</pre>") . dropWhile (~/= "<pre>") . last $ pres
    rootTextNodes = extractCurrentLevelTextNodes . mergeTextTags . replaceBr $ testsContainer
    inputsAndOutputs = filter (not . T.null) . map (T.strip . fromTagText) $ rootTextNodes
    testCases = [TestCase (inputsAndOutputs!!i) (inputsAndOutputs!!(i+1)) |
                    i <- [0, 2 .. length inputsAndOutputs-2]]

    probType = Stream StdIn StdOut


extractCurrentLevelTextNodes :: Eq a => [Tag a] -> [Tag a]
extractCurrentLevelTextNodes tags = go tags []
  where
    go [] nodes = reverse nodes
    go (TagText s:rest) nodes = go rest (TagText s:nodes)
    go (TagOpen name _ : rest) nodes = go (dropWhile (not . isTagCloseName name) rest) nodes
    go (_:rest) nodes = go rest nodes

replaceBr :: [Tag T.Text] -> [Tag T.Text]
replaceBr [] = []

replaceBr (o:c:rest)
    | isTagOpenName "br" o && isTagCloseName "br" c   = TagText "\n" : replaceBr rest

replaceBr (t:rest)
    | isTagName "br" t = TagText "\n" : replaceBr rest
    | otherwise        = t : replaceBr rest

doParseContest :: URL -> IO (Either T.Text [URL])
doParseContest url = parseChefContest <$> downloadDocument url

parseChefContest :: Either T.Text URL -> Either T.Text [T.Text]
parseChefContest (Left err)   = Left err
parseChefContest (Right cont) = if null problemsTable
                                    then Left "Couldn't parse contest"
                                    else Right problems
  where
    tags = parseTags cont
    problemsTable = takeWhile (~/= "</table>") . dropWhile (~~/== "<table class=problems>") $ tags
    problemCells = sections (~~== "<div class=problemname>") problemsTable
    problems = mapMaybe extractUrl problemCells


extractUrl :: [Tag T.Text] -> Maybe T.Text
extractUrl cell = T.append "http://codechef.com" <$> if null anchors then Nothing else Just url
  where
    anchors = dropWhile (~/= "<a>") cell
    anchor = head anchors
    url = fromAttrib (T.pack "href") anchor

