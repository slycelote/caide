{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
module Caide.CheckUpdates(
      parseLatestVersion
    , checkUpdates
) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (listToMaybe)
import Data.Text.Encoding (encodeUtf8)
import Data.Version (Version, parseVersion)
import GHC.Generics (Generic)
import Text.ParserCombinators.ReadP (readP_to_S)

import qualified Data.Aeson as Aeson

import Caide.GlobalState (GlobalState(latestVersion), modifyGlobalState, flushGlobalState)
import Caide.Types (CaideIO)
import Caide.Util (withLock)
import Network.HTTP.Util (downloadDocument)


data Release = Release
             { tag_name :: String
             , draft :: Bool
             , prerelease :: Bool
             } deriving (Generic, Show)

instance Aeson.FromJSON Release

parseLatestVersion :: LBS.ByteString -> Maybe Version
parseLatestVersion s = do
    releases :: [Release] <- Aeson.decode s
    let nonBetaReleases = [r | r <- releases, not (draft r || prerelease r)]
    latestRelease <- listToMaybe nonBetaReleases
    let version = tag_name latestRelease
    when (head version /= 'v') Nothing
    let parseResults = readP_to_S parseVersion $ tail version
        fullParseResults = [v | (v, "") <- parseResults]
    case fullParseResults of
        [v] -> Just v
        _   -> Nothing

checkUpdates :: CaideIO ()
checkUpdates = do
    releases <- liftIO $ downloadDocument "https://api.github.com/repos/slycelote/caide/releases"
    case releases of
        Left _ -> pure ()
        Right contents -> do
            let bsContents = LBS.fromStrict . encodeUtf8 $ contents
            case parseLatestVersion bsContents of
                Nothing  -> pure ()
                ver -> withLock $ do
                    modifyGlobalState $ \s -> s {latestVersion = ver}
                    flushGlobalState

