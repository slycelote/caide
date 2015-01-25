{-# LANGUAGE OverloadedStrings #-}
module Caide.Commands.ParseContest(
      createContest
) where

import Control.Monad (unless)
import Control.Monad.Except (catchError)
import Control.Monad.State (liftIO)
import Data.Maybe (catMaybes)
import qualified Data.Text as T

import Caide.Commands.ParseProblem (parseExistingProblem)
import Caide.Configuration (describeError)
import Caide.Registry (findProblemParser, findContestParser)
import Caide.Types


createContest :: URL -> CaideIO ()
createContest url = case findContestParser url of
    Nothing -> throw . T.concat $ [url, " is not recognized as a supported contest URL"]
    Just contestParser -> do
        parserRet <- liftIO $ contestParser `parseContest` url
        case parserRet of
            Left  err  -> throw err
            Right urls -> do
                results <- mapM tryParseProblem $ reverse urls
                let errors = catMaybes results
                unless (null errors) $
                    throw $ T.unlines ("Some problems failed to parse: ": errors)

tryParseProblem :: URL -> CaideIO (Maybe T.Text)
tryParseProblem url = case findProblemParser url of
    Nothing -> return . Just . T.concat $ ["Couldn't find problem parser for URL: ", url]
    Just parser -> (parseExistingProblem url parser >> return Nothing) `catchError`
                   (return . Just . T.pack . describeError)

