{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns #-}
module Caide.Monad(
      CaideIO
    , CaideM
    , RunSettings(..)
    , Verbosity(..)
    , run
    , caideRoot
    , caideVerbosity
    , caideSettings
    , caideHttpClient
    , caideHoldingLock
    , describeError

    , throw
    , orThrow
    , rightOrThrow

    , Feature (..)
    , noOpFeature
) where

import Control.Exception.Base (displayException)
import Control.Monad.Extended (ExceptT, MonadError, runExceptT, throwError, MonadIO, when)
import Control.Monad.Reader (MonadReader, reader, ReaderT, runReaderT)
import Data.Either.Util (mapLeft)
import Data.IORef (IORef, newIORef)
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FS
import System.IO.Error (tryIOError)

import qualified Caide.HttpClient as Http
import Caide.Settings (Settings(useFileLock), readSettings)
import Caide.Types


data Verbosity = Info | Debug
    deriving (Show, Enum, Ord, Eq, Bounded)

data RunSettings = RunSettings
    { root       :: !FS.FilePath
    , verbosity  :: !Verbosity
    , httpClient :: !Http.Client
    }

data CaideEnv = CaideEnv
    { runSettings :: !RunSettings
    -- Config file read from root directory
    , settings :: !Settings
    -- Used to avoid taking the file lock twice, which doesn't work on Windows
    , holdingFileLock :: !(Maybe (IORef Bool))
    }

newtype Error = Error T.Text
    deriving Show

newtype CaideM m a = CaideM { unCaideM :: ReaderT CaideEnv (ExceptT Error m) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadError Error, MonadReader CaideEnv)

type CaideIO a = CaideM IO a

runCaideM :: CaideM m a -> CaideEnv -> m (Either Error a)
runCaideM caideAction env = runExceptT $ runReaderT (unCaideM caideAction) env

describeError :: Error -> T.Text
describeError (Error text) = text

run :: RunSettings -> CaideIO a -> IO (Either Error a)
run runSettings caideAction = do
    let logEx e = do
            when (verbosity runSettings >= Debug) $
                putStrLn $ T.unpack $ describeError e
            pure (Left e)
    ret <- tryIOError $ do
        iniSettings <- mapLeft Error <$> readSettings (root runSettings)
        case iniSettings of
            Left e  -> pure $ Left e
            Right settings -> do
                holdingFileLock <- if useFileLock settings
                    then Just <$> newIORef False
                    else pure Nothing
                runCaideM caideAction CaideEnv{runSettings, settings, holdingFileLock}
    case ret of
        Left e -> logEx $ Error $ T.pack $ displayException e
        Right (Left e) -> logEx e
        Right (Right a) -> pure $ Right a

throw :: Monad m => T.Text -> CaideM m a
throw desc = throwError $ Error desc

orThrow :: Monad m => Either e a -> (e -> T.Text) -> CaideM m a
ea `orThrow` f = either (throw . f) pure ea

rightOrThrow :: Monad m => Either T.Text a -> CaideM m a
rightOrThrow ea = ea `orThrow` id

-- | Return root caide directory
caideRoot :: Monad m => CaideM m FS.FilePath
caideRoot = root <$> reader runSettings

caideVerbosity :: Monad m => CaideM m Verbosity
caideVerbosity = verbosity <$> reader runSettings

caideSettings :: Monad m => CaideM m Settings
caideSettings = reader settings

caideHttpClient :: CaideIO Http.Client
caideHttpClient = httpClient <$> reader runSettings

caideHoldingLock :: CaideIO (Maybe (IORef Bool))
caideHoldingLock = reader holdingFileLock


-- | A (legacy) feature is a piece of optional functionality that may be run
--   at certain points, depending on the configuration. A feature doesn't
--   run by itself, but only in response to certain events.
--   The first parameter in all functions is ID of the problem that triggered the event.
data Feature = Feature
    { onProblemCreated     :: ProblemID -> CaideIO ()   -- ^ Run after `caide problem`
    , onProblemCodeCreated :: ProblemID -> CaideIO ()   -- ^ Run after `caide lang`
    , onProblemCheckedOut  :: ProblemID -> CaideIO ()   -- ^ Run after `caide checkout`
    , onProblemRemoved     :: ProblemID -> CaideIO ()   -- ^ Run after `caide archive`
    }

noOpFeature :: Feature
noOpFeature =  Feature
    { onProblemCreated     = const $ return ()
    , onProblemCodeCreated = const $ return ()
    , onProblemCheckedOut  = const $ return ()
    , onProblemRemoved     = const $ return ()
    }

