{-# LANGUAGE OverloadedStrings #-}
module Caide.Commands.Checkout (
      checkoutProblem
) where

import Control.Monad (forM_, unless)
import Control.Monad.Except (catchError)
import Control.Monad.State (liftIO)
import Data.Maybe (mapMaybe)

import qualified Data.Text as T
import qualified Data.Text.IO.Util as T

import Filesystem (isFile)
import Filesystem.Path.CurrentOS ((</>))

import Caide.Commands.BuildScaffold (generateScaffoldSolution)
import Caide.Configuration (getActiveProblem, setActiveProblem)
import qualified Caide.GlobalTemplate as GlobalTemplate
import qualified Caide.Paths as Paths
import Caide.Registry (findFeature)
import Caide.Settings (enabledFeatureNames)
import Caide.Types
import Caide.Util (withLock)

checkoutProblem :: ProblemID -> Maybe T.Text -> CaideIO ()
checkoutProblem probId' maybeLangStr = unless (T.null probId') $ do
    root <- caideRoot
    let probId = T.dropAround (\c -> c == '/' || c == '\\') probId'
        probDir = Paths.problemDir root probId
    problemExists <- liftIO $ isFile $ probDir </> "problem.ini"

    unless problemExists $ throw . T.concat $ ["Problem ", probId, " doesn't exist"]

    currentProbId <- getActiveProblem `catchError` (\_ -> return "")

    if currentProbId == probId
        then liftIO $ T.putStrLn . T.concat $ [probId, ": already checked out"]
        else withLock $ do
            setActiveProblem probId
            liftIO $ T.putStrLn . T.concat $ ["Checked out problem ", probId]
            features <- mapMaybe findFeature . enabledFeatureNames <$> caideSettings
            forM_ (GlobalTemplate.hook : features) (`onProblemCheckedOut` probId)

    case maybeLangStr of
        Just langStr -> generateScaffoldSolution langStr
        Nothing      -> return ()

