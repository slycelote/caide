{-# LANGUAGE CPP, OverloadedStrings #-}
module Caide.Parsers.CodeforcesContest(
      codeforcesContestParser
) where

#ifndef AMP
import Control.Applicative ((<$>))
#endif
import Control.Monad.Except (liftIO)
import Data.Maybe (mapMaybe)

import qualified Data.Text as T
import Network.URI (parseURI, uriAuthority, uriRegName)

import Text.HTML.TagSoup (partitions, parseTags, fromAttrib, Tag)
import Text.HTML.TagSoup.Utils


import Caide.Commands.ParseProblem (parseProblems)
import Caide.Types
import Caide.Util (downloadDocument)


codeforcesContestParser :: ContestParser
codeforcesContestParser = ContestParser
    { contestUrlMatches = isCodeForcesUrl
    , parseContest = doParseContest
    }

isCodeForcesUrl :: URL -> Bool
isCodeForcesUrl url = case parseURI (T.unpack url) >>= uriAuthority of
    Nothing   -> False
    Just auth -> uriRegName auth `elem` ["codeforces.com", "www.codeforces.com", "codeforces.ru", "www.codeforces.ru"]

doParseContest :: URL -> CaideIO ()
doParseContest url = do
    maybeUrls <- liftIO $ parseCfContest <$> downloadDocument url
    case maybeUrls of
        Left err   -> throw err
        Right urls -> parseProblems 3  urls

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

