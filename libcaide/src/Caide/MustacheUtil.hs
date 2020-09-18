{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Caide.MustacheUtil(
      renderTemplates
    , renderTemplates'
    , compileTemplates
    , renderCompiledTemplates
    , RenderTemplatesOption(..)
    , enrich
) where


import Control.Exception (Exception(displayException))
import qualified Control.Exception as Exc
import Control.Exception.Base (try)
import Control.Monad (forM, forM_, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as LazyText
import qualified Data.Vector as Vector
import qualified System.IO.Error as IOError

import Filesystem (copyPermissions, isFile, removeFile, writeTextFile)
import Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem.Util as FS

import qualified Data.Aeson as Aeson

import Text.Microstache (MustacheException, Template, compileMustacheFile, displayMustacheWarning, renderMustacheW)

import Caide.Logger (logWarn)
import Caide.Types


data RenderTemplatesOption = AllowOverwrite Bool

data Options = Options
             { allowOverwrite :: Bool
             }

applyModifier :: RenderTemplatesOption -> Options -> Options
applyModifier option oldOptions = case option of
    AllowOverwrite b -> oldOptions{allowOverwrite=b}

combine :: [RenderTemplatesOption] -> Options
combine = foldr applyModifier $ Options{allowOverwrite=False}

renderTemplates' :: FS.FilePath -> FS.FilePath -> Aeson.Value -> CaideIO Int
renderTemplates' templatesDir targetDir context = renderTemplates templatesDir targetDir context []

renderTemplates :: FS.FilePath -> FS.FilePath -> Aeson.Value -> [RenderTemplatesOption] -> CaideIO Int
renderTemplates templatesDir targetDir context optionModifiers = do
    let options = combine optionModifiers
        enrichedContext = enrich context
    templates <- liftIO $ fst <$> FS.listDir templatesDir
    forM_ templates $ \templateFile -> do
        templateOrException <- liftIO $ try $ compileMustacheFile (FS.encodeString templateFile)
        template <- case templateOrException of
            Left e -> throw $ T.unwords
                ["Couldn't parse Mustache template in", FS.pathToText templateFile, ":", T.pack $ displayException (e :: MustacheException)]
            Right t -> return t
        renderTemplate templateFile template targetDir enrichedContext options
    pure $ length templates

compileTemplates :: FS.FilePath -> CaideIO [(FS.FilePath, Template)]
compileTemplates templatesDir = do
    templates <- liftIO $ fst <$> FS.listDir templatesDir
    forM templates $ \templateFile -> do
        templateOrException <- liftIO $ try $ compileMustacheFile (FS.encodeString templateFile)
        case templateOrException of
            Left e -> throw $ T.unwords
                ["Couldn't parse Mustache template in", FS.pathToText templateFile, ":", T.pack $ displayException (e :: MustacheException)]
            Right t -> pure (templateFile, t)


renderCompiledTemplates :: [(FS.FilePath, Template)] -> FS.FilePath -> Aeson.Value -> [RenderTemplatesOption] -> CaideIO ()
renderCompiledTemplates templates targetDir context optionModifiers = do
    let options = combine optionModifiers
        enrichedContext = enrich context
    forM_ templates $ \(templateFile, template) ->
        renderTemplate templateFile template targetDir enrichedContext options

ignoring :: Exception e => IO () -> (e -> Bool) -> IO ()
action `ignoring` predicate = Exc.catchJust
    (\e -> if predicate e then Just () else Nothing) action pure

renderTemplate :: FS.FilePath -> Template -> FS.FilePath -> Aeson.Value -> Options -> CaideIO ()
renderTemplate templateFile template targetDir enrichedContext (Options{allowOverwrite}) = do
    let newFileName = targetDir </> (FS.filename templateFile)
        (warnings, renderedTemplate) = renderMustacheW template enrichedContext
        warningTexts = map (T.pack . displayMustacheWarning) warnings
    keepExisting <- if allowOverwrite then pure False else liftIO $ isFile newFileName
    if keepExisting
        then logWarn $ "Keeping existing file " <> FS.pathToText newFileName
        else do
            unless (null warnings) $ logWarn $ T.unwords $
                ["Warning(s) for Mustache template", FS.pathToText templateFile, ":", T.intercalate "; " warningTexts]
            let strictText = LazyText.toStrict renderedTemplate
            liftIO $ if T.null $ T.strip strictText
                then removeFile newFileName `ignoring` IOError.isDoesNotExistError
                else do
                    writeTextFile newFileName strictText
                    copyPermissions templateFile newFileName


enrich :: Aeson.Value -> Aeson.Value
enrich (Aeson.Object hashMap) = Aeson.Object $ Map.union transformedMap nonEmptyIndicators
  where
    transformedMap = Map.map enrich hashMap
    keysWithNonEmptyArrayValues = [k | (k, Aeson.Array v) <- Map.toList transformedMap, not (Vector.null v)]
    nonEmptyIndicators = Map.fromList [(k <> "_nonempty", Aeson.Bool True) | k <- keysWithNonEmptyArrayValues]

enrich (Aeson.Array vector) = Aeson.Array $ Vector.imap addFirstLast transformedVector
  where
    transformedVector = Vector.map enrich vector

    addFirstLast :: Int -> Aeson.Value -> Aeson.Value
    addFirstLast idx (Aeson.Object hashMap) = Aeson.Object $ Map.union hashMap firstLastIndicators
      where
        firstLastIndicators = Map.fromList $ [("isfirst", Aeson.Bool True) | idx == 0] ++
                                             [("islast",  Aeson.Bool True) | idx == Vector.length vector - 1]
    addFirstLast _idx v = v

enrich v = v

