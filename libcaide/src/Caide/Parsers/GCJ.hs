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
import Text.HTML.TagSoup.Utils

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

    texts = map normalizeText .
            mapMaybe (maybeTagText . (!!1)) .
            sections (~~== "<pre class=io-content>") .
            dropWhile (~~/== "<div class=problem-io-wrapper>") $
            tags
    testCases = [TestCase (texts!!i) (texts!!(i+1)) | i <- [0, 2 .. length texts-2]]

    -- TODO: a special problem type for GCJ-like judges
    probType = Stream StdIn StdOut
    problem = (Problem title probId probType, testCases)

-- Replace \r\n with \n, strip
normalizeText :: T.Text -> T.Text
normalizeText = T.replace "\r\n" "\n" . T.strip

