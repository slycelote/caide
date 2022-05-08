{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.CodeforcesContest(
      codeforcesContestParser
) where

import Data.Maybe (mapMaybe)

import qualified Data.Text as T
import Data.Text (Text)

import Text.HTML.TagSoup (partitions, parseTags, fromAttrib, Tag)
import Text.HTML.TagSoup.Utils


import qualified Caide.HttpClient as Http
import Caide.Parsers.Common (URL, ContestParser(..), ContestParserResult(Urls),
    downloadDocument, isHostOneOf)


codeforcesContestParser :: ContestParser
codeforcesContestParser = ContestParser
    { contestUrlMatches = isCodeForcesUrl
    , parseContest = doParseContest
    }

isCodeForcesUrl :: URL -> Bool
isCodeForcesUrl = isHostOneOf ["codeforces.com", "www.codeforces.com", "codeforces.ru", "www.codeforces.ru", "codeforces.ml", "www.codeforces.ml"]

doParseContest :: Http.Client -> URL -> IO (Either Text ContestParserResult)
doParseContest client url = do
    maybeUrls <- parseCfContest <$> downloadDocument client url
    case maybeUrls of
        Left err   -> return $ Left err
        Right urls -> return $ Right $ Urls urls

parseCfContest :: Either T.Text URL -> Either T.Text [T.Text]
parseCfContest (Left err)   = Left err
parseCfContest (Right cont) = if null problemsTable
                              then Left "Couldn't parse contest"
                              else Right problems
  where
    tags = parseTags cont
    problemsTable = takeWhile (~/= "</table>") . dropWhile (~~/== "<table class=problems>") $ tags
    trs = partitions (~== "<tr>") problemsTable
    problems = mapMaybe extractURL trs

extractURL :: [Tag T.Text] -> Maybe T.Text
extractURL tr = T.append (T.pack "http://codeforces.com") <$> if null anchors then Nothing else Just url
  where
    td = dropWhile (~~/== "<td class=id>") tr
    anchors = dropWhile (~/= "<a>") td
    anchor = head anchors
    url = fromAttrib (T.pack "href") anchor

