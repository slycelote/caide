{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.RCC(
      htmlParser
    , chelperId
    , isSupportedUrl
    , parseProblemFromTags
) where

import Data.Char (isAlpha)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as T

import Text.HTML.TagSoup (Tag, maybeTagText, parseTags, sections)
import Text.HTML.TagSoup.Utils

import Caide.Parsers.Common (URL, isHostOneOf, normalizeText)
import Caide.Types

chelperId :: T.Text
chelperId = "rcc"

isSupportedUrl :: URL -> Bool
isSupportedUrl = isHostOneOf ["russiancodecup.ru", "www.russiancodecup.ru"]

htmlParser :: T.Text -> IO (Either T.Text (Problem, [TestCase]))
htmlParser = pure . parseProblemFromTags . parseTags

parseProblemFromTags :: [Tag T.Text] -> Either T.Text (Problem, [TestCase])
parseProblemFromTags tags =
    if null testCases
    then Left "Couldn't parse problem"
    else Right problem
  where
    titleText = drop 1 . dropWhile (~~/== "<div class=container>") .
        dropWhile (~~/== "<div class='blueBlock hTask'>") $ tags

    title' = listToMaybe titleText >>= maybeTagText
    probId' = T.snoc "" <$> (title' >>= T.find isAlpha)
    title  = fromMaybe "Unknown" title'
    probId = T.append "rcc" . fromMaybe "Unknown" $ probId'

    texts = map normalizeText .
            mapMaybe (maybeTagText . (!!1)) .
            sections (~~== "<pre class=colorBlue>") $
            tags
    testCases = [TestCase (texts!!i) (texts!!(i+1)) | i <- [0, 2 .. length texts-2]]

    probType = Stream StdIn StdOut
    problem = (makeProblem title probId probType, testCases)

