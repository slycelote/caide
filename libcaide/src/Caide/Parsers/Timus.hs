{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.Timus(
      htmlParser
    , isSupportedUrl
    , chelperId
) where

import Data.Function ((&))
import Data.List.Util (chunked)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as T

import Text.HTML.TagSoup (maybeTagText, parseTags, sections)
import Text.HTML.TagSoup.Utils

import Caide.Parsers.Common (URL, isHostOneOf, normalizeTestCases)
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

    texts = tags & dropWhile (~~/== "<table class=sample>") & takeWhile (~~/== "</table>") &
            sections (~~== "<pre>") & mapMaybe (maybeTagText . (!!1))

    testCases = texts & chunked & map (\(i, o) -> TestCase i (Just o)) & normalizeTestCases

    probType = Stream StdIn StdOut
    problem = (makeProblem title probId probType, testCases)

