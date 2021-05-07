{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.POJ(
      htmlParser
    , chelperId
    , isSupportedUrl
) where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as T

import Text.HTML.TagSoup (maybeTagText, parseTags, sections)
import Text.HTML.TagSoup.Utils

import Caide.Parsers.Common (URL, isHostOneOf, normalizeTestCases)
import Caide.Types

chelperId :: T.Text
chelperId = "poj"

isSupportedUrl :: URL -> Bool
isSupportedUrl = isHostOneOf ["poj.org", "www.poj.org"]

htmlParser :: T.Text -> IO (Either T.Text (Problem, [TestCase]))
htmlParser cont = pure $
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

    texts = mapMaybe (maybeTagText . (!!1)) .
            sections (~~== "<pre class=sio>") $
            tags
    testCases = normalizeTestCases [TestCase (texts!!i) (texts!!(i+1)) | i <- [0, 2 .. length texts-2]]

    probType = Stream StdIn StdOut
    problem = (makeProblem title probId probType, testCases)

