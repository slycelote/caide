{-# LANGUAGE OverloadedStrings #-}
module Caide.Commands.ParseContest (
      cmd
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


cmd :: CommandHandler
cmd = CommandHandler
    { command = "contest"
    , description = "Parse a contest"
    , usage = "caide contest <URL>"
    , action = doParseContest
    }

doParseContest :: [T.Text] -> CaideIO ()
doParseContest [url] = case findContestParser url of
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

doParseContest _ = throw . T.concat $ ["Usage: ", usage cmd]

tryParseProblem :: URL -> CaideIO (Maybe T.Text)
tryParseProblem url = case findProblemParser url of
    Nothing -> return . Just . T.concat $ ["Couldn't find problem parser for URL: ", url]
    Just parser -> (parseExistingProblem url parser >> return Nothing) `catchError`
                   (return . Just . T.pack . describeError)

