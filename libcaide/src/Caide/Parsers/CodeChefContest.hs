{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.CodeChefContest(
      codeChefContestParser
) where

import Control.Applicative ((<$>))
import Control.Monad.Except (liftIO)
import qualified Data.Text as T
import Data.Maybe (mapMaybe)
import Network.URI (parseURI, uriAuthority, uriRegName)

import Text.HTML.TagSoup (Tag, fromAttrib, sections, parseTags)
import Text.HTML.TagSoup.Utils ((~~==), (~~/==))

import Caide.Commands.ParseProblem (parseProblems)
import Caide.Types
import Caide.Util (downloadDocument)

codeChefContestParser :: ContestParser
codeChefContestParser = ContestParser
    { contestUrlMatches = isCodeChefUrl
    , parseContest = doParseContest
    }

isCodeChefUrl :: URL -> Bool
isCodeChefUrl url = case parseURI (T.unpack url) >>= uriAuthority of
    Nothing   -> False
    Just auth -> uriRegName auth `elem` ["codechef.com", "www.codechef.com"]

doParseContest :: URL -> CaideIO ()
doParseContest url = do
    maybeUrls <- liftIO $ parseChefContest <$> downloadDocument url
    case maybeUrls of
        Left err   -> throw err
        Right urls -> parseProblems 1  urls


parseChefContest :: Either T.Text URL -> Either T.Text [T.Text]
parseChefContest (Left err)   = Left err
parseChefContest (Right cont) = if null problemsTable
                                    then Left "Couldn't parse contest"
                                    else Right problems
  where
    tags = parseTags cont
    problemsTable = takeWhile (~~/== "</table>") . dropWhile (~~/== "<table class=problems>") $ tags
    problemCells = sections (~~== "<div class=problemname>") problemsTable
    problems = mapMaybe extractUrl problemCells


extractUrl :: [Tag T.Text] -> Maybe T.Text
extractUrl cell = T.append "http://codechef.com" <$> if null anchors then Nothing else Just url
  where
    anchors = dropWhile (~~/== "<a>") cell
    anchor = head anchors
    url = fromAttrib (T.pack "href") anchor

