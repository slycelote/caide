{-# LANGUAGE OverloadedStrings #-}
module Caide.Commands.Checkout (
      checkoutProblem
) where

import Control.Monad.Extended (forM_, liftIO, unless, whenJust)
import Data.Maybe (mapMaybe)

import qualified Data.Text as T
import qualified Data.Text.IO.Util as T

import Filesystem (isFile)
import Filesystem.Path.CurrentOS ((</>))

import Caide.Commands.BuildScaffold (generateScaffoldSolution)
import Caide.GlobalState (readGlobalState, writeGlobalState, activeProblem)
import qualified Caide.GlobalTemplate as GlobalTemplate
import Caide.Monad (CaideIO, caideRoot, caideSettings, Feature(onProblemCheckedOut), throw)
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

    unless problemExists $ throw $ "Problem " <> probId <> " doesn't exist"

    globalState <- readGlobalState

    if activeProblem globalState == Just probId
        then liftIO $ T.putStrLn $ probId <> ": already checked out"
        else withLock $ do
            writeGlobalState globalState{activeProblem = Just probId}
            liftIO $ T.putStrLn $ "Checked out problem " <> probId
            features <- mapMaybe findFeature . enabledFeatureNames <$> caideSettings
            forM_ (GlobalTemplate.hook : features) (`onProblemCheckedOut` probId)

    whenJust maybeLangStr generateScaffoldSolution

