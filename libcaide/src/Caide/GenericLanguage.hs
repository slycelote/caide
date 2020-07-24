{-# LANGUAGE OverloadedStrings #-}
module Caide.GenericLanguage(
      language
) where

import Control.Exception (Exception(displayException))
import Control.Exception.Base (try)
import Control.Monad (filterM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.IO as T
import Filesystem (copyPermissions, isFile, listDirectory, writeTextFile)
import Filesystem.Path.CurrentOS ((</>), filename, fromText, encodeString)
import Filesystem.Util (pathToText)

import Text.Mustache (MustacheException, compileMustacheFile, displayMustacheWarning, renderMustacheW)

import Caide.Logger (logWarn)
import Caide.Types (CaideIO, ProblemID, ProgrammingLanguage(..), caideRoot, throw)
import Caide.Problem (jsonEncodeProblem, readProblemInfo)

language :: Text -> ProgrammingLanguage
language languageName = ProgrammingLanguage
    { generateScaffold = genericScaffold languageName
    , inlineCode = genericInlineCode languageName
    }

genericScaffold :: Text -> ProblemID -> CaideIO ()
genericScaffold languageName probId = do
    root <- caideRoot
    let problemDir = root </> fromText probId
        templatesDir = root </> fromText "templates" </> fromText languageName
    itemsInTemplatesDir <- liftIO $ listDirectory templatesDir
    templates <- liftIO $ filterM isFile itemsInTemplatesDir
    when (null templates) $
        throw $ T.unwords ["No templates for", languageName, "found in", pathToText templatesDir]
    forM_ templates $ \templateFile -> do
        templateOrException <- liftIO $ try $ compileMustacheFile (encodeString templateFile)
        template <- case templateOrException of
            Left e -> throw $ T.unwords
                ["Couldn't parse Mustache template in", pathToText templateFile, ":", T.pack $ displayException (e :: MustacheException)]
            Right t -> return t
        problem <- readProblemInfo probId
        let baseFileName = filename templateFile
            newFileName = problemDir </> baseFileName
            (warnings, renderedTemplate) = renderMustacheW template (jsonEncodeProblem problem)
            warningTexts = map (T.pack . displayMustacheWarning) warnings
        fileExists <- liftIO $ isFile newFileName
        if fileExists
            then logWarn $ "Not overwriting existing file " <> pathToText newFileName
            else do
                unless (null warnings) $ logWarn $ T.unwords $
                    ["Warning(s) for Mustache template", pathToText baseFileName, ":", T.intercalate "; " warningTexts]
                liftIO $ do
                    writeTextFile newFileName (LazyText.toStrict renderedTemplate)
                    copyPermissions templateFile newFileName


genericInlineCode :: Text -> ProblemID -> CaideIO ()
genericInlineCode languageName _probId = throw $ "Code inlining is not supported for " <> languageName

