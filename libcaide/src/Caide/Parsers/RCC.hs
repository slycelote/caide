{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.RCC(
      rccParser
    , parseRccProblem
) where

import Control.Applicative ((<$>))
import Data.Char (isAlpha)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as T
import Network.URI (parseURI, uriAuthority, uriRegName)

import Text.HTML.TagSoup (Tag, maybeTagText, parseTags, sections)
import Text.HTML.TagSoup.Utils

import Caide.Parsers.Common (normalizeText)
import Caide.Types

rccParser :: HtmlParser
rccParser = HtmlParser
    { chelperId = "rcc"
    , htmlParserUrlMatches = isRccUrl
    , parseFromHtml = parseRccProblem . parseTags
    }

isRccUrl :: URL -> Bool
isRccUrl url = case parseURI (T.unpack url) of
    Nothing   -> False
    Just uri  -> (uriRegName <$> uriAuthority uri) `elem` map Just ["russiancodecup.ru", "www.russiancodecup.ru"]


parseRccProblem :: [Tag T.Text] -> Either T.Text (Problem, [TestCase])
parseRccProblem tags =
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
    problem = (Problem title probId probType, testCases)

