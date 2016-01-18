{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.Timus(
      timusParser
) where

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as T
import Network.URI (parseURI, uriAuthority, uriRegName)

import Text.HTML.TagSoup (maybeTagText, parseTags, sections)
import Text.HTML.TagSoup.Utils

import Caide.Parsers.Common (normalizeText)
import Caide.Types

timusParser :: HtmlParser
timusParser = HtmlParser
    { chelperId = "timus"
    , htmlParserUrlMatches = isTimusUrl
    , parseFromHtml = doParse
    }

isTimusUrl :: URL -> Bool
isTimusUrl url = case parseURI (T.unpack url) >>= uriAuthority of
    Nothing   -> False
    Just auth -> uriRegName auth == "acm.timus.ru"


doParse :: T.Text -> Either T.Text (Problem, [TestCase])
doParse cont =
    if null testCases
    then Left "Couldn't parse problem"
    else Right problem
  where
    tags = parseTags cont

    h2Title = drop 1 . dropWhile (~~/== "<h2 class=problem_title>") $ tags
    title' = listToMaybe h2Title >>= maybeTagText
    probId' = T.takeWhile (/= '.') <$> title'
    title  = fromMaybe "Unknown" title'
    probId = T.append "timus" . fromMaybe "Unknown" $ probId'

    texts = map normalizeText .
            mapMaybe (maybeTagText . (!!1)) .
            sections (~~== "<pre>") .
            takeWhile (~~/== "</table>") .
            dropWhile (~~/== "<table class=sample>") $
            tags
    testCases = [TestCase (texts!!i) (texts!!(i+1)) | i <- [0, 2 .. length texts-2]]

    probType = Stream StdIn StdOut
    problem = (Problem title probId probType, testCases)

