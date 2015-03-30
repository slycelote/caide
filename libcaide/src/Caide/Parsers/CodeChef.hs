{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.CodeChef(
      codeChefParser
) where

import qualified Data.Text as T

import Network.URI (parseURI, uriAuthority, uriRegName)

import Text.HTML.TagSoup (Tag(..), innerText, fromTagText, parseTags,
                         isTagCloseName, sections)

import Caide.Parsers.Common (mergeTextTags, replaceBr)
import Caide.Types
import Text.HTML.TagSoup.Utils ((~==), (~/=), (~~/==))


codeChefParser :: HtmlParser
codeChefParser = HtmlParser
    { chelperId = "codechef"
    , htmlParserUrlMatches = isCodeChefUrl
    , parseFromHtml = doParse
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

