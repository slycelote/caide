{-# LANGUAGE OverloadedStrings #-}
module Caide.MustacheUtil(
      generateTemplates
) where


import Control.Exception (Exception(displayException))
import Control.Exception.Base (try)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LazyText
import Filesystem (copyPermissions, isFile, writeTextFile)
import Filesystem.Path.CurrentOS ((</>), filename, fromText)
import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem.Util as FS
import qualified Data.Aeson as Aeson
import Text.Mustache (MustacheException, compileMustacheFile, displayMustacheWarning, renderMustacheW)

import Caide.Logger (logWarn)
import Caide.Types


generateTemplates :: FS.FilePath -> FS.FilePath -> Aeson.Value -> CaideIO ()
generateTemplates templatesDir targetDir value = do
    templates <- liftIO $ fst <$> FS.listDir templatesDir
    when (null templates) $
        throw $ T.unwords ["No templates found in", FS.pathToText templatesDir]
    forM_ templates $ \templateFile -> do
        templateOrException <- liftIO $ try $ compileMustacheFile (FS.encodeString templateFile)
        template <- case templateOrException of
            Left e -> throw $ T.unwords
                ["Couldn't parse Mustache template in", FS.pathToText templateFile, ":", T.pack $ displayException (e :: MustacheException)]
            Right t -> return t
        let baseFileName = filename templateFile
            newFileName = targetDir </> baseFileName
            (warnings, renderedTemplate) = renderMustacheW template value
            warningTexts = map (T.pack . displayMustacheWarning) warnings
        fileExists <- liftIO $ isFile newFileName
        if fileExists
            then logWarn $ "Not overwriting existing file " <> FS.pathToText newFileName
            else do
                unless (null warnings) $ logWarn $ T.unwords $
                    ["Warning(s) for Mustache template", FS.pathToText baseFileName, ":", T.intercalate "; " warningTexts]
                liftIO $ do
                    writeTextFile newFileName (LazyText.toStrict renderedTemplate)
                    copyPermissions templateFile newFileName


