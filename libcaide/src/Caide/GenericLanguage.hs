{-# LANGUAGE OverloadedStrings #-}
module Caide.GenericLanguage(
      makeLanguage
) where

import Control.Monad.Extended (liftIO, when)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Filesystem.Path.CurrentOS ((</>), (<.>))
import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem.Util as FS

import Caide.MustacheUtil (renderTemplates, RenderTemplatesOption(AllowOverwrite))
import Caide.Monad (CaideIO, caideRoot, caideSettings, throw, rightOrThrow)
import qualified Caide.Paths as Paths
import Caide.Problem (jsonEncodeProblem, readProblemInfo, readProblemState)
import Caide.Settings (getLanguage, getExtension)
import Caide.Types (ProblemID)
import Caide.Types.ProgrammingLanguage (ProgrammingLanguage(..))

makeLanguage :: Text -> ProgrammingLanguage
makeLanguage languageName = ProgrammingLanguage
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
    numTemplates <- renderTemplates templatesDir probDir
        (jsonEncodeProblem problem problemState)
        [AllowOverwrite False]
    when (numTemplates == 0) $
        throw $ "No templates found for language '" <> languageName <> "'"

filterTestingCode :: T.Text -> T.Text
filterTestingCode = T.unlines . filterLines . T.lines
  where
    beginMarker = "BEGIN TESTING CODE"
    endMarker = "END TESTING CODE"
    filterLines fileLines = go (0 :: Int) [] fileLines
    go _ acc [] = reverse acc
    go level acc (line:rest)
        | beginMarker `T.isInfixOf` line  = go (level + 1)       acc rest
        | endMarker `T.isInfixOf` line    = go (max 0 $ level-1) acc rest
        | level == 0    = go level (line:acc) rest
        | otherwise     = go level acc rest

genericInlineCode :: Text -> ProblemID -> CaideIO ()
genericInlineCode languageName probId = do
    root <- caideRoot
    settings <- caideSettings
    let probDir = root </> Paths.problemDir root probId
    (files, _) <- liftIO $ FS.listDir probDir
    let ext = getExtension $ getLanguage settings languageName
        solutionFiles = files & filter (\path ->
            let fileName = FS.pathToText $ FS.filename path
            in path `FS.hasExtension` ext &&
               not ("_test" `T.isSuffixOf` fileName) &&
               not ("test_" `T.isPrefixOf` fileName)
            )
        submissionFile = root </> "submission" <.> ext

    solutionFiles &
        mapM (liftIO . FS.readTextFile) >>=
        mapM rightOrThrow <&>
        map filterTestingCode <&>
        intersperse "\n" <&>
        T.concat >>=
        (liftIO . FS.writeTextFile submissionFile)

