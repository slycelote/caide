{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
module Caide.CheckUpdates(
      parseLatestVersion
    , checkUpdates
    , logIfUpdateAvailable
    , checkUpdatesCommand
) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (listToMaybe)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDay)
import Data.Version (Version, parseVersion)
import GHC.Generics (Generic)
import Text.ParserCombinators.ReadP (readP_to_S)

import qualified Data.Aeson as Aeson

import Paths_libcaide (version)
import Caide.Configuration (readCaideConf, orDefault)
import Caide.GlobalState (GlobalState(latestVersion, lastUpdateCheck), readGlobalState, modifyGlobalState, flushGlobalState)
import Caide.Logger (logInfo, logWarn)
import Caide.Types (CaideIO, getProp, throw)
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
    let tagName = tag_name latestRelease
    when (head tagName /= 'v') Nothing
    let parseResults = readP_to_S parseVersion $ tail tagName
        fullParseResults = [v | (v, "") <- parseResults]
    case fullParseResults of
        [v] -> Just v
        _   -> Nothing

checkUpdatesImpl :: CaideIO ()
checkUpdatesImpl = do
    releases <- liftIO $ downloadDocument "https://api.github.com/repos/slycelote/caide/releases"
    case releases of
        Left e -> throw e
        Right contents -> do
            let bsContents = LBS.fromStrict . encodeUtf8 $ contents
            case parseLatestVersion bsContents of
                Nothing -> throw "Couldn't parse latest version"
                ver -> do
                    t <- liftIO getCurrentTime
                    withLock $ do
                        modifyGlobalState $ \s -> s {latestVersion = ver, lastUpdateCheck = Just t}
                        flushGlobalState

checkUpdates :: CaideIO ()
checkUpdates = do
    h <- readCaideConf
    checkEnabled <- getProp h "core" "check_updates" `orDefault` True
    s <- readGlobalState
    t <- liftIO getCurrentTime
    let checkedRecently = case lastUpdateCheck s of
            Nothing -> False
            Just t1 -> diffUTCTime t t1 < 7 * nominalDay
    when (checkEnabled && not checkedRecently) checkUpdatesImpl

checkUpdatesCommand :: CaideIO ()
checkUpdatesCommand = do
    checkUpdatesImpl
    haveUpdates <- isUpdateAvailable
    logInfo $ if haveUpdates
              then "New version is available on GitHub: https://github.com/slycelote/caide/releases."
              else "Caide is up to date."

isUpdateAvailable :: CaideIO Bool
isUpdateAvailable = do
    s <- readGlobalState
    pure $ case latestVersion s of
        Just v | v > version -> True
        _ -> False

logIfUpdateAvailable :: CaideIO ()
logIfUpdateAvailable = do
    haveUpdates <- isUpdateAvailable
    when haveUpdates $ logWarn "New version is available on GitHub: https://github.com/slycelote/caide/releases"

