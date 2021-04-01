{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.Codeforces(
      htmlParser
    , chelperId
    , isSupportedUrl
) where

import Data.Either (rights)
import qualified Data.Text as T

import Filesystem.Path.CurrentOS (fromText)

import Text.HTML.TagSoup (fromTagText, innerText, parseTags, sections, fromAttrib)
import Text.HTML.TagSoup.Utils

import Text.Parsec
import Text.Parsec.Text (Parser)

import Caide.Parsers.Common (URL, isHostOneOf, normalizeTestCases, replaceBr)
import Caide.Types


chelperId :: T.Text
chelperId = "codeforces"

isSupportedUrl :: URL -> Bool
isSupportedUrl = isHostOneOf ["codeforces.com", "www.codeforces.com", "codeforces.ru", "www.codeforces.ru"]

htmlParser :: T.Text -> IO (Either T.Text (Problem, [TestCase]))
htmlParser cont = pure $
    if null beforeTitleDiv
    then Left "Couldn't parse problem statement"
    else Right (makeProblem title probId probType, normalizeTestCases testCases)
  where
    tags = parseTags cont
    statement = dropWhile (~~/== "<div class=problem-statement>") tags
    beforeTitleDiv = drop 1 . dropWhile (~~/== "<div class=title>") $ statement
    title = fromTagText $ head beforeTitleDiv
    inputDivs = sections (~~== "<div class=input>") statement
    outputDivs = sections (~~== "<div class=output>") statement

    extractText = T.dropWhile (`elem` ['\r', '\n']) . innerText . replaceBr . takeWhile (~/= "</pre>") . dropWhile (~/= "<pre>")
    inputs = map extractText inputDivs
    outputs = map extractText outputDivs
    testCases = zipWith TestCase inputs outputs

    -- Contest
    sidebar = dropWhile (~/= "<div id=sidebar>") tags
    rtable = takeWhile (~/= "</table>") . dropWhile (~~/== "<table class=rtable>") $ sidebar
    anchors = sections (~== "<a>") rtable
    links = map (fromAttrib "href" . head) anchors
    contestIds = rights . map (parse contestUrlLink "link") $ links

    probIdPrefix = if length contestIds == 1
                   then T.append "cf" (head contestIds)
                   else "cfproblem"
    probId = T.append probIdPrefix (T.takeWhile (/= '.') title)

    inputFileDiv = dropWhile (~~/== "<div class=input-file") statement
    inputFileName = innerText . takeWhile (~~/== "</div>") . drop 1 . dropWhile (~~/== "</div>" ) $ inputFileDiv

    outputFileDiv = dropWhile (~~/== "<div class=output-file") statement
    outputFileName = innerText . takeWhile (~~/== "</div>") . drop 1 . dropWhile (~~/== "</div>" ) $ outputFileDiv

    inputSource = if T.toLower inputFileName `elem` ["standard input", "стандартный ввод"]
                     then StdIn
                     else FileInput $ fromText inputFileName

    outputTarget = if T.toLower outputFileName `elem` ["standard output", "стандартный вывод"]
                      then StdOut
                      else FileOutput $ fromText outputFileName

    probType = Stream inputSource outputTarget

contestUrlLink :: Parser T.Text
contestUrlLink = do
    _ <- string "/contest/"
    contId <- many1 digit
    eof
    return $ T.pack contId

