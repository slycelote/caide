{-# LANGUAGE OverloadedStrings #-}
module Caide.Commands.ParseContest(
      createContest
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_, unless)
import Control.Monad.State (liftIO)
import Data.Either (rights)
import qualified Data.Text as T

import Caide.Commands.ParseProblem (saveProblem)
import Caide.Registry (findProblemParser, findContestParser)
import Caide.Types


createContest :: URL -> CaideIO ()
createContest contestUrl = case findContestParser contestUrl of
    Nothing -> throw . T.concat $ [contestUrl, " is not recognized as a supported contest URL"]
    Just contestParser -> do
        parserRet <- liftIO $ contestParser `parseContest` contestUrl
        case parserRet of
            Left  err  -> throw err
            Right urls -> do
                results <- liftIO $ mapConcurrently tryParseProblem urls
                let errors = [T.concat [url, ": ", err] | (url, Left err) <- zip urls results]
                    problems = rights results
                forM_ (reverse problems) $ uncurry saveProblem
                unless (null errors) $
                    throw $ T.unlines ("Some problems failed to parse.": errors)


tryParseProblem :: URL -> IO (Either T.Text (Problem, [TestCase]))
tryParseProblem url = case findProblemParser url of
    Nothing -> return . Left . T.concat $ ["Couldn't find problem parser for URL: ", url]
    Just parser -> parser `parseProblem` url

