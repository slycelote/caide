{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Caide.Settings (
      Settings(..)
    , CppSettings(..)
    , readSettings
) where

import Data.Text (Text)

import qualified Filesystem.Path.CurrentOS as FS

import Caide.Configuration (readConfigFile, getPropOrDefault)
import qualified Caide.Paths as Paths

data Settings = Settings
              { autoCheckUpdates :: !Bool
              , chelperPort :: !(Maybe Int)
              , companionPort :: !(Maybe Int)
              , defaultLanguage :: !Text
              , verboseTestReport :: !Bool
              , enabledTemplateNames :: ![Text]
              , cppSettings :: !CppSettings
              , useFileLock :: !Bool
              , enabledFeatureNames :: ![Text] -- legacy 'features'
              } deriving (Show)

data CppSettings = CppSettings
    { clangOptions :: ![Text]
    , keepMacros :: ![Text]
    , maxConsecutiveEmptyLines :: !Int
    } deriving (Show)

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
        useFileLock <- getOpt "core" "use_lock" True
        enabledFeatureNames <- getOpt "core" "features" []

        clangOptions <- getOpt "cpp" "clang_options" []
        keepMacros <- getOpt "cpp" "keep_macros" ["ONLINE_JUDGE"]
        maxConsecutiveEmptyLines <- getOpt "cpp" "max_consequent_empty_lines" 2
        let cppSettings = CppSettings{..}

        pure $ Settings{..}
