{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.RccContest(
      rccContestParser
) where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.Except (liftIO)
import Data.Either (rights)
import qualified Data.Text as T
import Network.URI (parseURI, uriAuthority, uriRegName)

import Text.HTML.TagSoup (parseTags, partitions)
import Text.HTML.TagSoup.Utils

import Caide.Types
import Caide.Parsers.RCC (parseRccProblem)
import Caide.Commands.ParseProblem (saveProblem)
import Caide.Util (downloadDocument)


rccContestParser :: ContestParser
rccContestParser =  ContestParser
    { contestUrlMatches = isRccUrl
    , parseContest = doParse
    }

isRccUrl :: URL -> Bool
isRccUrl url = case parseURI (T.unpack url) of
    Nothing   -> False
    Just uri  -> (uriRegName <$> uriAuthority uri) `elem` map Just ["russiancodecup.ru", "www.russiancodecup.ru"]


doParse :: URL -> CaideIO ()
doParse contestUrl = do
    doc <- liftIO $ downloadDocument contestUrl
    case doc of
        Left err -> throw err
        Right cont -> case parseRccContest cont of
            [] -> throw "Couldn't find RCC problems at this URL"
            problems -> forM_ (reverse problems) $ uncurry saveProblem


parseRccContest :: T.Text -> [(Problem, [TestCase])]
parseRccContest doc = problems
  where
    tags = parseTags doc
    problemDivs = partitions (~~== "<div class=hTask>") tags
    problems = rights . map parseRccProblem $ problemDivs

