{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module Caide.GlobalState (
      GlobalState(..)
    , readGlobalState
    , writeGlobalState
    , modifyGlobalState
    , flushGlobalState
) where

import Data.Version (Version, makeVersion, versionBranch)
import GHC.Generics (Generic)
import qualified Data.Text as T

import qualified Data.Aeson as Aeson

import Caide.Configuration (readCaideState, orDefault)
import Caide.Types (CaideIO, ProblemID, getProp, setProp, flushConf)

data GlobalState = GlobalState
                 { activeProblem :: Maybe ProblemID
                 , latestVersion :: Maybe Version
                 } deriving (Generic, Show)

instance Aeson.FromJSON GlobalState
instance Aeson.ToJSON GlobalState

readGlobalState :: CaideIO GlobalState
readGlobalState = do
    h <- readCaideState
    mbActiveProblem <- getProp h "core" "problem" `orDefault` ""
    mbLatestVersion <- getProp h "core" "latest_version" `orDefault` []
    let activeProblem = if T.null mbActiveProblem then Nothing else Just mbActiveProblem
        latestVersion = case mbLatestVersion of
            [] -> Nothing
            v  -> Just $ makeVersion v
    pure $ GlobalState{..}

writeGlobalState :: GlobalState -> CaideIO ()
writeGlobalState GlobalState{..} = do
    h <- readCaideState
    case activeProblem of
        Just probId -> setProp h "core" "problem" probId
        _ -> pure ()
    case latestVersion of
        Just v -> setProp h "core" "latest_version" (versionBranch v)
        _ -> pure ()

modifyGlobalState :: (GlobalState -> GlobalState) -> CaideIO ()
modifyGlobalState f = do
    s <- readGlobalState
    writeGlobalState $ f s

flushGlobalState :: CaideIO ()
flushGlobalState = do
    h <- readCaideState
    flushConf h

