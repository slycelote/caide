{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.GCJ(
      gcjParser
) where

import Control.Applicative ((<$>))
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as T
import Network.URI (parseURI, uriAuthority, uriPath, uriRegName)

import Text.HTML.TagSoup (maybeTagText, parseTags, sections)
import Text.HTML.TagSoup.Match (tagOpenAttrNameLit)
import Text.HTML.TagSoup.Utils

import Caide.Parsers.Common (normalizeText)
import Caide.Types

gcjParser :: HtmlParser
gcjParser = HtmlParser
    { chelperId = "gcj"
    , htmlParserUrlMatches = isGcjUrl
    , parseFromHtml = doParse
    }

isGcjUrl :: URL -> Bool
isGcjUrl url = case parseURI (T.unpack url) of
    Nothing   -> False
    Just uri  -> (uriRegName <$> uriAuthority uri) == Just "code.google.com" &&
                 "/codejam/contest" `isPrefixOf` uriPath uri


{-# ANN doParse ("HLint: ignore Use head" :: String) #-}
doParse :: T.Text -> Either T.Text (Problem, [TestCase])
doParse cont =
    if null testCases
    then Left "Couldn't parse problem"
    else Right problem
  where
    tags = parseTags cont

    titleText = drop 1 . dropWhile (~~/== "<div class=dynamic-link>") .
        dropWhile (~~/== "<div class=dsb-problem-tab-open>") $ tags

    title' = listToMaybe titleText >>= maybeTagText
    probId' = T.takeWhile (/= '.') <$> title'
    title  = fromMaybe "Unknown" title'
    probId = T.append "gcj" . fromMaybe "Unknown" $ probId'

    isActiveTab t = tagOpenAttrNameLit "div" "style" ("block" `T.isInfixOf`) t &&
                    t ~~== "<div class=dsb-content-pages>"
    activeTab = dropWhile (not . isActiveTab) . dropWhile (~~/== "<div id=dsb-problem-pages") $ tags
    texts = map normalizeText .
            mapMaybe (maybeTagText . (!!1)) .
            sections (~~== "<pre class=io-content>") .
            dropWhile (~~/== "<div class=problem-io-wrapper>") $
            activeTab
    testCases = [TestCase (texts!!0) (texts!!1) | length texts >= 2]

    -- TODO: a special problem type for GCJ-like judges
    probType = Stream StdIn StdOut
    problem = (Problem title probId probType, testCases)

