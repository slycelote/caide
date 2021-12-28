{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.Yandex(
      htmlParser
    , isSupportedUrl
    , chelperId
) where

import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as T

import Text.HTML.TagSoup (maybeTagText, parseTags, partitions, sections, Tag)
import Text.HTML.TagSoup.Utils

import Caide.Parsers.Common (URL, normalizeTestCases, isHostOneOf)
import Caide.Types

chelperId :: T.Text
chelperId = "yandex"

isSupportedUrl :: URL -> Bool
isSupportedUrl = isHostOneOf ["contest.yandex.ru", "www.contest.yandex.ru", "contest.yandex.com", "www.contest.yandex.com"]

htmlParser :: T.Text -> IO (Either T.Text (Problem, [TestCase]))
htmlParser cont = pure $
    if null testCases
    then Left "Couldn't parse problem"
    else Right problem
  where
    tags = parseTags cont

    spanTitle = drop 1 . dropWhile (~~/== "<span>") .
                dropWhile (~~/== "<li class='tabs-menu__tab_active_yes'>") .
                dropWhile (~~/== "<ul class='tabs-menu_role_problems'>") $ tags
    title' = T.strip <$> (listToMaybe spanTitle >>= maybeTagText)
    probId' = T.take 1 <$> title'

    title  = fromMaybe "Unknown" title'
    probId = T.append "yandex" . fromMaybe "Unknown" $ probId'

    testCases = concatMap extractTestCase .
                partitions (~~== "<table class=sample-tests>") $
                tags

    probType = Stream StdIn StdOut
    problem = (makeProblem title probId probType, testCases)

extractTestCase :: [Tag T.Text] -> [TestCase]
extractTestCase tags = testCases
  where
    texts = mapMaybe maybeTagText .
            mapMaybe (listToMaybe . drop 1) .
            sections (~~== "<pre>") $
            tags

    testCases = normalizeTestCases [TestCase (texts!!i) (Just $ texts!!(i+1)) | i <- [0, 2 .. length texts-2]]

