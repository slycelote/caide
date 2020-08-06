{-# LANGUAGE CPP, OverloadedStrings #-}
module Caide.Parsers.POJ(
      pojParser
) where

#ifndef AMP
import Control.Applicative ((<$>))
#endif
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as T
import Network.URI (parseURI, uriAuthority, uriRegName)

import Text.HTML.TagSoup (maybeTagText, parseTags, sections)
import Text.HTML.TagSoup.Utils

import Caide.Parsers.Common (normalizeText)
import Caide.Types

pojParser :: HtmlParser
pojParser = HtmlParser
    { chelperId = "poj"
    , htmlParserUrlMatches = isPojUrl
    , parseFromHtml = doParse
    }

isPojUrl :: URL -> Bool
isPojUrl url = case parseURI (T.unpack url) >>= uriAuthority of
    Nothing   -> False
    Just auth -> uriRegName auth `elem` ["poj.org", "www.poj.org"]


doParse :: T.Text -> Either T.Text (Problem, [TestCase])
doParse cont =
    if null testCases
    then Left "Couldn't parse problem"
    else Right problem
  where
    tags = parseTags cont

    divTitle = drop 1 . dropWhile (~~/== "<div class=ptt>") $ tags
    title' = listToMaybe divTitle >>= maybeTagText

    titleTag = drop 1 . dropWhile (~~/== "<title>") $ tags
    pidAndTitle = listToMaybe titleTag >>= maybeTagText
    probId' = T.takeWhile isDigit <$> pidAndTitle

    title  = fromMaybe "Unknown" title'
    probId = T.append "poj" . fromMaybe "Unknown" $ probId'

    texts = map normalizeText .
            mapMaybe (maybeTagText . (!!1)) .
            sections (~~== "<pre class=sio>") $
            tags
    testCases = [TestCase (texts!!i) (texts!!(i+1)) | i <- [0, 2 .. length texts-2]]

    probType = Stream StdIn StdOut
    problem = (makeProblem title probId probType, testCases)

