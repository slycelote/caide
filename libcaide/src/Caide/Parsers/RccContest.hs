{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.RccContest(
      rccContestParser
) where

import Control.Monad (forM_)
import Control.Monad.Except (liftIO)
import Data.Either (rights)
import qualified Data.Text as T

import Text.HTML.TagSoup (parseTags, partitions)
import Text.HTML.TagSoup.Utils

import Caide.Types
import Caide.Parsers.Common (URL, ContestParser(..), isHostOneOf)
import Caide.Parsers.RCC (parseProblemFromTags)
import Caide.Commands.ParseProblem (saveProblemWithScaffold)
import Caide.Util (downloadDocument)


rccContestParser :: ContestParser
rccContestParser =  ContestParser
    { contestUrlMatches = isRccUrl
    , parseContest = doParse
    }

isRccUrl :: URL -> Bool
isRccUrl = isHostOneOf ["russiancodecup.ru", "www.russiancodecup.ru"]

doParse :: URL -> CaideIO ()
doParse contestUrl = do
    doc <- liftIO $ downloadDocument contestUrl
    case doc of
        Left err -> throw err
        Right cont -> case parseRccContest cont of
            [] -> throw "Couldn't find RCC problems at this URL"
            problems -> forM_ (reverse problems) $ uncurry saveProblemWithScaffold


parseRccContest :: T.Text -> [(Problem, [TestCase])]
parseRccContest doc = problems
  where
    tags = parseTags doc
    problemDivs = partitions (~~== "<div class=hTask>") tags
    problems = rights . map parseProblemFromTags $ problemDivs

