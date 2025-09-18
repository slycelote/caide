{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module Caide.GlobalState (
      GlobalState(..)
    , readGlobalState
    , writeGlobalState
    , modifyGlobalState
    , noActiveProblemError
) where

import Control.Monad.Extended (liftIO, catchError)
import Control.Monad.State (execStateT)
import qualified Data.ByteString.Lazy.Char8 as AsciiLBS
import Data.Maybe (fromMaybe)
import Data.Version (Version, makeVersion, versionBranch)
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)

import qualified Data.Aeson as Aeson

import Filesystem.Path.CurrentOS((</>))

import Caide.Configuration (readConfigFile, writeConfigFile, getProp, getPropOrDefault, putProp)
import Caide.Monad (CaideIO, caideRoot, orThrow, rightOrThrow)
import qualified Caide.Paths as Paths
import Caide.Types (ProblemID)


-- | Global state of caide directory, stored in .caide/config
data GlobalState = GlobalState
                 { activeProblem :: !(Maybe ProblemID)
                 , latestVersion :: !(Maybe Version)
                 , lastUpdateCheck :: !(Maybe UTCTime)
                 } deriving (Generic, Show)

instance Aeson.FromJSON GlobalState
instance Aeson.ToJSON GlobalState

readGlobalState :: CaideIO GlobalState
readGlobalState = do
    root <- caideRoot
    mbcp <- liftIO $ readConfigFile root Paths.caideStateFile
    cp <- mbcp `orThrow` \e -> "Couldn't read state file .caide/config: " <> e
    rightOrThrow $ do
        mbActiveProblem <- getPropOrDefault cp "core" "problem" ""
        mbLatestVersion <- getPropOrDefault cp "core" "latest_version" []
        lastUpdateCheck <- ((Aeson.decode . AsciiLBS.pack . T.unpack) <$> getProp cp "core" "last_update") `catchError` const (pure Nothing)
        let activeProblem = if T.null mbActiveProblem then Nothing else Just mbActiveProblem
            latestVersion = case mbLatestVersion of
                [] -> Nothing
                v  -> Just $ makeVersion v
        pure $ GlobalState{..}

writeGlobalState :: GlobalState -> CaideIO ()
writeGlobalState GlobalState{..} = do
    root <- caideRoot
    mbcp <- liftIO $ readConfigFile root Paths.caideStateFile
    cp <- rightOrThrow $ do
        cp <- mbcp
        flip execStateT cp $ do
            putProp "core" "problem" $ fromMaybe "" activeProblem
            case latestVersion of
                Just v -> putProp "core" "latest_version" $ versionBranch v
                _ -> pure ()
            case lastUpdateCheck of
                Just t -> putProp "core" "last_update" $ T.pack $ AsciiLBS.unpack $ Aeson.encode $ t
                _ -> pure ()

    liftIO $ writeConfigFile cp $ root </> Paths.caideStateFile

modifyGlobalState :: (GlobalState -> GlobalState) -> CaideIO ()
modifyGlobalState f = do
    s <- readGlobalState
    writeGlobalState $ f s

noActiveProblemError :: T.Text
noActiveProblemError = "No active problem. Switch to an existing problem with `caide checkout <problemID>`"
