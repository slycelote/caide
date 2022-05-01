{-# LANGUAGE OverloadedStrings #-}
module Caide.GenericLanguage(
      language
) where

import Control.Monad (when)
import Data.Text (Text)
import Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path.CurrentOS as FS

import Caide.MustacheUtil (renderTemplates, RenderTemplatesOption(AllowOverwrite))
import qualified Caide.Paths as Paths
import Caide.Types (CaideIO, ProblemID, ProgrammingLanguage(..), caideRoot, throw)
import Caide.Problem (jsonEncodeProblem, readProblemInfo, readProblemState)

language :: Text -> ProgrammingLanguage
language languageName = ProgrammingLanguage
    { generateScaffold = genericScaffold languageName
    , inlineCode = genericInlineCode languageName
    }


genericScaffold :: Text -> ProblemID -> CaideIO ()
genericScaffold languageName probId = do
    root <- caideRoot
    let probDir = Paths.problemDir root probId
        templatesDir = root </> FS.fromText "templates" </> FS.fromText languageName
    problem <- readProblemInfo probId
    problemState <- readProblemState probId
    numTemplates <- renderTemplates templatesDir probDir (jsonEncodeProblem problem problemState) [AllowOverwrite False]
    when (numTemplates == 0) $
        throw $ "No templates found for " <> languageName


genericInlineCode :: Text -> ProblemID -> CaideIO ()
genericInlineCode languageName _probId = throw $ "Code inlining is not supported for " <> languageName

