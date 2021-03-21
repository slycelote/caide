{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Caide.Settings (
      Settings(..)
    , readSettings
) where

import Control.Monad.Error.Class (catchError, throwError)
import Data.Text (Text)
import qualified Data.Text as T

import Data.ConfigFile (ConfigParser(..), CPErrorData(..), CPError,
    get, set, emptyCP, interpolatingAccess,
    readfile)
import qualified Filesystem.Path as F
import qualified Filesystem.Path.CurrentOS as F

import Caide.Types.Option (Option(optionFromString))

data Settings = Settings
              { autoCheckUpdates :: !Bool
              , chelperPort :: !(Maybe Int)
              , companionPort :: !(Maybe Int)
              , defaultLanguage :: !Text
              , verboseTestReport :: !Bool
              , enabledTemplateNames :: ![Text]
              , useFileLock :: !Bool
              , enabledFeatureNames :: ![Text] -- legacy 'features'
              } deriving (Show)

readSettings :: F.FilePath -> IO (Either Text Settings)
readSettings filePath = do
    -- TODO: Verify that readfile works with Unicode
    cp <- readfile emptyCP $ F.encodeString filePath :: IO (Either CPError ConfigParser)
    let res = parseSettings filePath =<< cp :: Either CPError Settings
    pure $ mapLeft describeCPError res

parseSettings :: F.FilePath -> ConfigParser -> Either CPError Settings
parseSettings rootDir cp = do
    -- Compatibility
    cp' <- set cp "DEFAULT" "caideRoot" $ F.encodeString rootDir
    let conf = cp' { accessfunc = interpolatingAccess 10, usedefault = True }

        noOptionHandler :: a -> CPError -> Either CPError a
        noOptionHandler a (NoSection _, _) = pure a
        noOptionHandler a (NoOption _, _) = pure a
        noOptionHandler _ e = throwError e

        getOpt :: Option a => String -> String -> a -> Either CPError a
        getOpt section key def = flip catchError (\e -> noOptionHandler def e) $ do
            r <- get conf section key
            case optionFromString r of
                Just res -> pure res
                Nothing  -> throwError (ParseError ("Couldn't parse value '" <> r <> "'"), "")

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

    pure $ Settings{..}


describeCPError :: (CPErrorData, String) -> Text
describeCPError (errorData, location) = T.pack location <> ": " <> describe errorData

describe :: CPErrorData -> Text
describe (ParseError s) = T.pack s
describe (SectionAlreadyExists section) = "Duplicate section " <> T.pack section
describe (NoSection section) = "No section " <> T.pack section
describe (NoOption option) = "No option " <> T.pack option
describe (OtherProblem s) = T.pack s
describe (InterpolationError s) = T.pack s

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a)  = Left (f a)
mapLeft _ (Right c) = Right c

