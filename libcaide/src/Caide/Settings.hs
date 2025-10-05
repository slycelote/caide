{-# LANGUAGE OverloadedStrings, NamedFieldPuns, RecordWildCards #-}
module Caide.Settings (
      Settings(..)
    , Language(langName)
    , getLanguage
    , getExtension
    , CppSettings(..)
    , readSettings
) where

import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Filesystem.Path.CurrentOS as FS

import qualified Data.ConfigFile as CF

import Caide.Configuration (readConfigFile, getPropOptional, getPropOrDefault)
import qualified Caide.Paths as Paths

data Settings = Settings
              { autoCheckUpdates :: !Bool
              , chelperPort :: !(Maybe Int)
              , companionPort :: !(Maybe Int)
              , defaultLanguage :: !Text
              , verboseTestReport :: !Bool
              -- | Global templates
              , enabledTemplateNames :: ![Text]
              -- | Configuration for programming languages
              , languages :: Map.Map Text Language
              , cppSettings :: !CppSettings
              , color :: !(Maybe Bool)
              , useFileLock :: !Bool
              , enabledFeatureNames :: ![Text] -- legacy 'features'
              } deriving (Show)

data CppSettings = CppSettings
    { clangOptions :: ![Text]
    , keepMacros :: ![Text]
    , maxConsecutiveEmptyLines :: !Int
    } deriving (Show)

-- | Configuration for a programming language.
-- Zero-configuration is possible, so all field except for name are
-- optional.
data Language = Language
    { langName :: !Text
    -- | File extension of source files of the language.
    -- By default, language name is used.
    , langExtension :: !(Maybe T.Text)
    } deriving (Show)

getLanguage :: Settings -> T.Text -> Language
getLanguage Settings{languages} name =
    fromMaybe Language{langName=name, langExtension=Nothing} $
        Map.lookup name languages

getExtension :: Language -> Text
getExtension Language{langName, langExtension} = fromMaybe langName langExtension

readSettings :: FS.FilePath -> IO (Either Text Settings)
readSettings caideRoot = do
    mbcp <- readConfigFile caideRoot Paths.caideConfFile
    pure $ do
        cp <- mbcp
        let getOpt section key def = getPropOrDefault cp section key def
        autoCheckUpdates <- getOpt "core" "check_updates" True
        chelperPort' <- getOpt "core" "chelper_port" 4243
        let chelperPort = if chelperPort' > 0 then Just chelperPort' else Nothing
        companionPort' <- getOpt "core" "companion_port" 10043
        let companionPort = if companionPort' > 0 then Just companionPort' else Nothing
        defaultLanguage <- getOpt "core" "language" "cpp"
        verboseTestReport <- getOpt "core" "verbose_test_report" False
        enabledTemplateNames <- getOpt "core" "templates" []
        color <- getPropOptional cp "core" "color"
        useFileLock <- getOpt "core" "use_lock" True
        enabledFeatureNames <- getOpt "core" "features" []

        let sectionNames = [s | s <- CF.sections cp, "language." `isPrefixOf` s]
            parseLanguage section = do
                let langName = T.pack $ drop (T.length "language.") section
                langExtension <- getPropOptional cp section "extension"
                pure $ Language{langName, langExtension}

        languageList <- mapM parseLanguage sectionNames
        let languages = Map.fromList [(langName l, l) | l <- languageList]

        clangOptions <- getOpt "cpp" "clang_options" []
        keepMacros <- getOpt "cpp" "keep_macros" ["ONLINE_JUDGE"]
        maxConsecutiveEmptyLines <- getOpt "cpp" "max_consequent_empty_lines" 2
        let cppSettings = CppSettings{..}

        pure $ Settings{..}
