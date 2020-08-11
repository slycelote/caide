{-# LANGUAGE CPP, OverloadedStrings #-}
module Caide.Commands.Checkout (
      checkoutProblem
) where

#ifndef AMP
import Control.Applicative ((<$>))
#endif
import Control.Monad (forM_, unless)
import Control.Monad.Except (catchError)
import Control.Monad.State (liftIO)
import Data.Maybe (mapMaybe)

import qualified Data.Text as T
import qualified Data.Text.IO.Util as T

import Filesystem (isFile)
import Filesystem.Path.CurrentOS (fromText, (</>))

import Caide.Commands.BuildScaffold (generateScaffoldSolution)
import Caide.Configuration (getActiveProblem, setActiveProblem, getFeatures)
import qualified Caide.GlobalTemplate as GlobalTemplate
import Caide.Registry (findFeature)
import Caide.Types
import Caide.Util (withLock)

checkoutProblem :: ProblemID -> Maybe T.Text -> CaideIO ()
checkoutProblem probId' maybeLangStr = unless (T.null probId') $ do
    root <- caideRoot
    let probId = T.dropAround (\c -> c == '/' || c == '\\') probId'
        problemDir = root </> fromText probId
    problemExists <- liftIO $ isFile $ problemDir </> "problem.ini"

    unless problemExists $ throw . T.concat $ ["Problem ", probId, " doesn't exist"]

    currentProbId <- getActiveProblem `catchError` (\_ -> return "")

    if currentProbId == probId
        then liftIO $ T.putStrLn . T.concat $ [probId, ": already checked out"]
        else withLock $ do
            setActiveProblem probId
            liftIO $ T.putStrLn . T.concat $ ["Checked out problem ", probId]
            features <- mapMaybe findFeature <$> getFeatures
            forM_ (GlobalTemplate.hook : features) (`onProblemCheckedOut` probId)

    case maybeLangStr of
        Just langStr -> generateScaffoldSolution langStr
        Nothing      -> return ()

