{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Caide.GlobalTemplate(
      regenerateAll
    , hook
) where

import Prelude hiding (FilePath)
import Control.Monad (filterM, forM, forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as List

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson

import Filesystem.Path.CurrentOS ((</>), FilePath)
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem.Util as FS

import Caide.MustacheUtil (compileTemplates, renderCompiledTemplates, renderTemplates,
    RenderTemplatesOption(AllowOverwrite))
import qualified Caide.Paths as Paths
import Caide.Monad (CaideIO, caideRoot, Feature(..), noOpFeature, caideSettings)
import Caide.Problem (readProblemInfo, readProblemState, jsonEncodeProblem)
import Caide.Settings (enabledTemplateNames)
import Caide.Types

isProblem :: FilePath -> IO Bool
isProblem dir = FS.isFile $ dir </> "problem.ini"

getProblems :: FilePath -> IO [ProblemID]
getProblems root = do
    dirs <- snd <$> FS.listDir root
    dirs' <- filterM (\d -> isProblem (root </> d)) dirs
    pure $ List.sort $ map (FS.pathToText . FS.filename) dirs'

encodeProblem :: ProblemID -> CaideIO Aeson.Value
encodeProblem probId = jsonEncodeProblem <$> readProblemInfo probId <*> readProblemState probId

regenerateAll :: CaideIO ()
regenerateAll = do
    root <- caideRoot
    problemIds <- liftIO $ getProblems root
    encodedProblems <- mapM encodeProblem problemIds
    let context = Aeson.object
            [ "problems" .= encodedProblems
            ]

    templateNames <- enabledTemplateNames <$> caideSettings

    forM_ templateNames $ \templateName -> do
        renderTemplates (root </> "templates" </> FS.fromText templateName) root context [AllowOverwrite True]

    compiledPerProblemTemplates <- fmap concat $ forM templateNames $ \templateName ->
        compileTemplates (root </> "templates" </> FS.fromText templateName </> "problem")

    forM_ (zip problemIds encodedProblems) $ \(probId, encodedProblem) ->
        renderCompiledTemplates compiledPerProblemTemplates (Paths.problemDir root probId) encodedProblem [AllowOverwrite True]


-- TODO: don't use the hook, just call regenerateAll once?
hook :: Feature
hook = noOpFeature
     { onProblemCodeCreated = const regenerateAll
     , onProblemCheckedOut = const regenerateAll
     , onProblemRemoved = const regenerateAll
     }

