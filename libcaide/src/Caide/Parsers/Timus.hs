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

import Caide.Types
import Caide.Util (downloadDocument)

timusParser :: ProblemParser
timusParser = ProblemParser
    { problemUrlMatches = isTimusUrl
    , parseProblem = doParse
    }

isTimusUrl :: URL -> Bool
isTimusUrl url = case parseURI (T.unpack url) >>= uriAuthority of
    Nothing   -> False
    Just auth -> uriRegName auth == "acm.timus.ru"


doParse :: URL -> IO (Either T.Text (Problem, [TestCase]))
doParse url = parseTimusProblem <$> downloadDocument url

parseTimusProblem :: Either T.Text T.Text -> Either T.Text (Problem, [TestCase])
parseTimusProblem (Left err)   = Left err
parseTimusProblem (Right cont) = if null testCases
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
            sections (~~== "<pre class=intable>") $
            tags
    testCases = [TestCase (texts!!i) (texts!!(i+1)) | i <- [0, 2 .. length texts-2]]

    probType = Stream StdIn StdOut
    problem = (Problem title probId probType, testCases)

-- Replace \r\n with \n, strip
normalizeText :: T.Text -> T.Text
normalizeText = T.replace "\r\n" "\n" . T.strip

