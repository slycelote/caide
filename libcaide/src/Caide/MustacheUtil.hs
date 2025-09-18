{-# LANGUAGE NamedFieldPuns, OverloadedStrings, ScopedTypeVariables #-}
module Caide.MustacheUtil(
      renderTemplates
    , renderTemplates'
    , compileTemplates
    , renderCompiledTemplates
    , RenderTemplatesOption(..)
    , enrich
    , compileAndRender
) where


import qualified Control.Exception.Extended as Exc
import Control.Monad.Extended (forM, forM_, liftIO, unless)
import qualified Data.Char as Char
import Data.Either.Util (mapLeft)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList)
import Data.Semigroup (sconcat)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Vector as Vector
import qualified System.IO.Error as IOError

import Data.Scientific as Scientific
import Filesystem (copyPermissions, isDirectory, isFile, removeFile, writeTextFile)
import Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem.Util as FS

import qualified Data.Aeson as Aeson
import Data.Aeson.Key (fromText, toText)
import qualified Data.Aeson.KeyMap as AesonMap
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as Parsec

import Text.Microstache (MustacheException, Template(templateActual), PName(PName),
    compileMustacheFile, compileMustacheText, displayMustacheWarning, renderMustacheW)

import Caide.Logger (logWarn)
import Caide.Monad (CaideIO, throw)
import Caide.Util (tshow)


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
        templateOrException <- liftIO $ Exc.try $ compileMustacheFile (FS.encodeString templateFile)
        template <- case templateOrException of
            Left (e :: MustacheException) -> throw $ T.unwords
                ["Couldn't parse Mustache template in", FS.pathToText templateFile, ":", T.pack $ Exc.displayException e]
            Right t -> return t
        renderTemplate templateFile template targetDir enrichedContext options
    pure $ length templates

compileTemplates :: FS.FilePath -> CaideIO [(FS.FilePath, Template)]
compileTemplates templatesDir = do
    isDir <- liftIO $ isDirectory templatesDir
    templates <- if isDir
        then liftIO $ fst <$> FS.listDir templatesDir
        else return []
    forM templates $ \templateFile -> do
        templateOrException <- liftIO $ Exc.try $ compileMustacheFile (FS.encodeString templateFile)
        case templateOrException of
            Left (e :: MustacheException) -> throw $ T.unwords
                ["Couldn't parse Mustache template in", FS.pathToText templateFile, ":", T.pack $ Exc.displayException e]
            Right t -> pure (templateFile, t)


renderCompiledTemplates :: [(FS.FilePath, Template)] -> FS.FilePath -> Aeson.Value -> [RenderTemplatesOption] -> CaideIO ()
renderCompiledTemplates templates targetDir context optionModifiers = do
    let options = combine optionModifiers
        enrichedContext = enrich context
    forM_ templates $ \(templateFile, template) ->
        renderTemplate templateFile template targetDir enrichedContext options

renderTemplate :: FS.FilePath -> Template -> FS.FilePath -> Aeson.Value -> Options -> CaideIO ()
renderTemplate templateFile template targetDir enrichedContext (Options{allowOverwrite}) = do
    let newFileName = targetDir </> (FS.filename templateFile)
        (warnings, renderedTemplate) = renderMustacheW template enrichedContext
        warningTexts = map (T.pack . displayMustacheWarning) warnings
    keepExisting <- if allowOverwrite then pure False else liftIO $ isFile newFileName
    if keepExisting
        then logWarn $ "Keeping existing file " <> FS.pathToText newFileName
        else do
            unless (null warnings) $ logWarn $ T.unwords
                ["Warning(s) for Mustache template", FS.pathToText templateFile, ":", T.intercalate "; " warningTexts]
            let strictText = LazyText.toStrict renderedTemplate
            liftIO $ if T.null $ T.strip strictText
                then removeFile newFileName `Exc.ignoring` IOError.isDoesNotExistError
                else do
                    writeTextFile newFileName strictText
                    copyPermissions templateFile newFileName


enrich :: Aeson.Value -> Aeson.Value
enrich (Aeson.Object hashMap) = Aeson.Object $ transformedMap <> nonEmptyIndicators <> equalsIndicators
  where
    transformedMap = AesonMap.map enrich hashMap
    keysWithNonEmptyArrayValues =
        [toText k | (k, Aeson.Array v) <- AesonMap.toList transformedMap, not (Vector.null v)]
    nonEmptyIndicators = AesonMap.fromList
        [(fromText k <> "_nonempty", Aeson.Bool True) | k <- keysWithNonEmptyArrayValues]
    keysForEqualsIndicators =
        [(toText k <> "_is_" <> t) | (k, Aeson.String t) <- AesonMap.toList transformedMap] <>
        [(toText k <> "_is_" <> tshow (i :: Int)) |
            (k, Aeson.Number n) <- AesonMap.toList transformedMap,
            i <- maybeToList (Scientific.toBoundedInteger n)]
    equalsIndicators = AesonMap.fromList
        [(fromText k, Aeson.Bool True) | k <- keysForEqualsIndicators, T.all isIdentifier k]
    isIdentifier c = Char.isAlphaNum c || c == '_'


enrich (Aeson.Array vector) = Aeson.Array $ Vector.imap addFirstLast transformedVector
  where
    transformedVector = Vector.map enrich vector

    addFirstLast :: Int -> Aeson.Value -> Aeson.Value
    addFirstLast idx (Aeson.Object hashMap) = Aeson.Object $ hashMap <> firstLastIndicators
      where
        firstLastIndicators = AesonMap.fromList
            [ ("isfirst", Aeson.Bool (idx == 0))
            , ("islast",  Aeson.Bool (idx == Vector.length vector - 1))
            ]
    addFirstLast _idx v = v

enrich v = v

-- A version of compileMustacheText that returns errors as text, including name as location.
compileMustacheText' :: Text -> Text -> Either Text Template
compileMustacheText' name templateText = templateText & LazyText.fromStrict &
    compileMustacheText (PName name) & mapLeft (\err -> tshow $
        Parsec.setErrorPos (Parsec.setSourceName (Parsec.errorPos err) (T.unpack name)) err)

compileAndRender :: NE.NonEmpty (Text, Text) -> NE.NonEmpty Text -> Aeson.Value -> Either Text (Maybe Text, NE.NonEmpty Text)
compileAndRender templatesWithNames primaryTemplateNames json = do
    compiledTemplates <- forM templatesWithNames $ \(name, text) -> compileMustacheText' name text
    let template = sconcat compiledTemplates
        enrichedValue = enrich json
        results = primaryTemplateNames <&> \name ->
            renderMustacheW template{templateActual = PName name} enrichedValue
        renderedTexts = NE.map (LazyText.toStrict . snd) results
        warnings = concatMap fst results
        warningTexts = case warnings of
            [] -> Nothing
            _  -> Just $ T.intercalate "; " $ map (T.pack . displayMustacheWarning) warnings
    return (warningTexts, renderedTexts)

