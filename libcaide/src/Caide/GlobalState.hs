{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module Caide.GlobalState (
      GlobalState(..)
    , readGlobalState
    , writeGlobalState
    , modifyGlobalState
    , noActiveProblemError
) where

import Control.Monad.Except (catchError)
import qualified Data.ByteString.Lazy.Char8 as AsciiLBS
import Data.Maybe (fromMaybe)
import Data.Version (Version, makeVersion, versionBranch)
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)

import qualified Data.Aeson as Aeson

import Caide.Configuration (readCaideState, orDefault)
import Caide.Types (CaideIO, ProblemID, getProp, setProp, flushConf)

data GlobalState = GlobalState
                 { activeProblem :: !(Maybe ProblemID)
                 , latestVersion :: !(Maybe Version)
                 , lastUpdateCheck :: !(Maybe UTCTime)
                 } deriving (Generic, Show)

instance Aeson.FromJSON GlobalState
instance Aeson.ToJSON GlobalState

readGlobalState :: CaideIO GlobalState
readGlobalState = do
    h <- readCaideState
    mbActiveProblem <- getProp h "core" "problem" `orDefault` ""
    mbLatestVersion <- getProp h "core" "latest_version" `orDefault` []
    lastUpdateCheck <- ((Aeson.decode . AsciiLBS.pack . T.unpack) <$> getProp h "core" "last_update") `catchError` const (pure Nothing)
    let activeProblem = if T.null mbActiveProblem then Nothing else Just mbActiveProblem
        latestVersion = case mbLatestVersion of
            [] -> Nothing
            v  -> Just $ makeVersion v
    pure $ GlobalState{..}

writeGlobalState :: GlobalState -> CaideIO ()
writeGlobalState GlobalState{..} = do
    h <- readCaideState
    setProp h "core" "problem" $ fromMaybe "" activeProblem
    case latestVersion of
        Just v -> setProp h "core" "latest_version" (versionBranch v)
        _ -> pure ()
    case lastUpdateCheck of
        Just t -> setProp h "core" "last_update" $ T.pack . AsciiLBS.unpack . Aeson.encode $ t
        _ -> pure ()
    flushConf h

modifyGlobalState :: (GlobalState -> GlobalState) -> CaideIO ()
modifyGlobalState f = do
    s <- readGlobalState
    writeGlobalState $ f s

noActiveProblemError :: T.Text
noActiveProblemError = "No active problem. Switch to an existing problem with `caide checkout <problemID>`"
