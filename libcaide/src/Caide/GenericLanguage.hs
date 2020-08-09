{-# LANGUAGE OverloadedStrings #-}
module Caide.GenericLanguage(
      language
) where

import Data.Text (Text)
import Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path.CurrentOS as FS

import Caide.MustacheUtil (generateTemplates)
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
    let problemDir = root </> FS.fromText probId
        templatesDir = root </> FS.fromText "templates" </> FS.fromText languageName
    problem <- readProblemInfo probId
    generateTemplates templatesDir problemDir (jsonEncodeProblem problem)


genericInlineCode :: Text -> ProblemID -> CaideIO ()
genericInlineCode languageName _probId = throw $ "Code inlining is not supported for " <> languageName

