{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.Timus(
      htmlParser
    , isSupportedUrl
    , chelperId
) where

import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as T

import Text.HTML.TagSoup (maybeTagText, parseTags, sections)
import Text.HTML.TagSoup.Utils

import Caide.Parsers.Common (URL, isHostOneOf, normalizeText)
import Caide.Types

chelperId :: T.Text
chelperId = "timus"

isSupportedUrl :: URL -> Bool
isSupportedUrl = isHostOneOf [ "acm.timus.ru" ]

htmlParser :: T.Text -> IO (Either T.Text (Problem, [TestCase]))
htmlParser cont = pure $
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
    problem = (makeProblem title probId probType, testCases)

