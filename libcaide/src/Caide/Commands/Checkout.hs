{-# LANGUAGE OverloadedStrings #-}
module Caide.Commands.Checkout (
      cmd
) where

import Control.Applicative ((<$>))
import Control.Monad (forM_, unless)
import Control.Monad.State (liftIO)
import Data.Maybe (mapMaybe)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Filesystem (isDirectory)
import Filesystem.Path.CurrentOS (fromText, (</>))

import Caide.Configuration (setActiveProblem, getActiveProblem, getFeatures, orDefault)
import Caide.Registry (findFeature)
import Caide.Types

cmd :: CommandHandler
cmd = CommandHandler
    { command = "checkout"
    , description = "Change active problem"
    , usage = "caide checkout <problem id>"
    , action = checkoutProblem
    }

checkoutProblem :: [T.Text] -> CaideIO ()
checkoutProblem [probId] = do
    root <- caideRoot
    let problemDir = root </> fromText probId
    problemExists <- liftIO $ isDirectory problemDir

    unless problemExists $ throw . T.concat $ ["Problem ", probId, " doesn't exist"]

    currentProbId <- getActiveProblem `orDefault` ""
    if currentProbId == probId
        then liftIO $ T.putStrLn . T.concat $ [probId, ": already checked out"]
        else do
            setActiveProblem probId
            liftIO $ T.putStrLn . T.concat $ ["Checked out problem ", probId]
            features <- mapMaybe findFeature <$> getFeatures
            forM_ features (`onProblemCheckedOut` probId)

checkoutProblem _ = throw . T.concat $ ["Usage: ", usage cmd]

