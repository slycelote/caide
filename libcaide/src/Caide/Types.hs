{-# LANGUAGE Rank2Types, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Caide.Types(
      Problem (..)
    , ProblemID

    , CaideIO
    , CaideM
    , runCaideM
    , runInDirectory

    , throw
    , assert

    , Option
    , Config
    , ConfigFileHandle
    , readConf
    , createConf
    , flushConf
    , getProp
    , setProp
    , caideRoot

    , ProblemParser (..)
    , ContestParser (..)
    , ProgrammingLanguage (..)
    , TestCase (..)
    , URL
    , CommandHandler (..)
    , Builder
    , BuilderResult(..)
    , Feature (..)
) where

import Control.Applicative (Applicative)
import Control.Monad (forM_, unless, when)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.State (StateT, MonadState, runStateT, gets, modify')
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text, pack)
import qualified Data.ConfigFile as C

import qualified Filesystem as F
import qualified Filesystem.Path as F
import qualified Filesystem.Path.CurrentOS as F

data TestCase = TestCase
    { testCaseInput  :: !Text
    , testCaseOutput :: !Text
    } deriving (Show)

type ProblemID = String

data Problem = Problem
    { problemName :: !Text      -- ^ Human readable identifier, used for displaying in GUI
    , problemId   :: !ProblemID -- ^ ID used for folder names, code generation etc.
    } deriving (Show)

type URL = Text

-- | Downloads problem data
data ProblemParser = ProblemParser
    { problemUrlMatches :: URL -> Bool
    , parseProblem      :: URL -> IO (Either String (Problem, [TestCase]))
    }

data ContestParser = ContestParser
    { contestUrlMatches :: URL -> Bool
    , parseContest      :: URL -> IO (Either String [URL])
    }


-- | The type encapsulating functions required to support particular target
-- programming language. The second argument to all functions is the path to directory
-- where the problem is located.
data ProgrammingLanguage = ProgrammingLanguage
    { generateScaffold   :: F.FilePath -> CaideIO ()
    , inlineCode         :: F.FilePath -> CaideIO ()
    }


class C.Get_C a => Option a where
    optionToString :: a -> String

instance Option Bool where
    optionToString False = "no"
    optionToString True  = "yes"

instance Option [Char] where
    optionToString = id

instance Option Double where
    optionToString = show


type Config = C.ConfigParser

newtype Monad m => CaideM m a = CaideM { unCaideM :: StateT CaideState (ExceptT C.CPError m) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadError C.CPError, MonadState CaideState)

type CaideIO a = CaideM IO a

runCaideM :: Monad m => CaideM m a -> CaideState -> m (Either C.CPError (a, CaideState))
runCaideM caideAction p = runExceptT $ runStateT (unCaideM caideAction) p

runInDirectory :: F.FilePath -> CaideIO a -> IO (Either C.CPError a)
runInDirectory dir caideAction = do
    let initialState = CaideState { root = dir, files = M.empty }
    ret <- runCaideM caideAction initialState
    case ret of
        Left e -> return $ Left e
        Right (a, finalState) -> do
            forM_ [f | f <- M.assocs (files finalState), modified (snd f)] $ uncurry writeConfigParser
            return $ Right a

-- | File handle through which it's possible to access options. See 'setProp', 'getProp'
newtype ConfigFileHandle = FileHandle F.FilePath

throw :: Monad m => String -> CaideM m a
throw desc = throwError (C.OtherProblem desc, "")

assert :: Monad m => Bool -> String -> CaideM m ()
assert condition message = unless condition $ throw message

-- | Return root caide directory
caideRoot :: Monad m => CaideM m F.FilePath
caideRoot = gets root

-- | Creates a new config file. Throws if it already exists.
createConf :: Monad m => F.FilePath -> C.ConfigParser -> CaideM m ConfigFileHandle
createConf filePath cp = do
    fileMap <- gets files
    when (M.member filePath fileMap) $
        throw $ "File " ++ F.encodeString filePath ++ " already exists"
    modifyWithFiles $ M.insert filePath ConfigInMemory{ configParser=cp, modified=True }
    return $ FileHandle filePath

-- | Reads a config file
readConf :: F.FilePath -> CaideIO ConfigFileHandle
readConf filePath = do
    fileMap <- gets files
    unless (M.member filePath fileMap) $ do
        conf <- readConfigFile filePath
        let newFile = ConfigInMemory { configParser = conf, modified = False}
        modifyWithFiles $ M.insert filePath newFile
    return $ FileHandle filePath

flushConf :: ConfigFileHandle -> CaideIO ()
flushConf (FileHandle filePath) = do
    fileMap <- gets files
    case M.lookup filePath fileMap of
        Nothing   -> throw $ "File " ++ F.encodeString filePath ++ " doesn't exist"
        Just conf -> liftIO $ writeConfigParser filePath conf


getProp :: (Monad m, Option a) => ConfigFileHandle -> String -> String -> CaideM m a
getProp (FileHandle path) section key = do
    mf <- gets (M.lookup path . files)
    case mf of
        Nothing -> throw $ "Unknown file handle " ++ F.encodeString path
        Just f  -> convertToCaide $ C.get (configParser f) section key

setProp :: (Monad m, Option a) => ConfigFileHandle -> String -> String -> a -> CaideM m ()
setProp (FileHandle path) section key value = do
    mf <- gets (M.lookup path . files)
    case mf of
        Nothing -> throw $ "Unknown file handle " ++ F.encodeString path
        Just f  -> do
            newConf <- convertToCaide $ C.set (configParser f) section key (optionToString value)
            let newFile = ConfigInMemory { configParser = newConf, modified = True}
            modifyWithFiles $ M.insert path newFile


-- | Describes caide command requested by the user, such as `init` or `test`
data CommandHandler = CommandHandler
    { command      :: String
    , description  :: String
    , usage        :: String
    , action       :: [String] -> CaideIO ()
    }

-- | 'Builder' result
data BuilderResult = BuildFailed  -- ^ Build failed or program under test exited unexpectedly
                   | TestsFailed  -- ^ Build succeeded, tests have been evaluated and failed
                   | NoEvalTests  -- ^ Build succeeded, tests have not been evaluated
                   | TestsPassed  -- ^ Tests succeeded

-- | Builder is responsible for building the code and running test program
type Builder = ProblemID -> CaideIO BuilderResult

-- | A feature is a piece of optional functionality that may be run
--   at certain points, depending on the configuration. A feature doesn't
--   run by itself, but only in response to certain events.
--   The first parameter in all functions is ID of the problem that triggered the event.
data Feature = Feature
    { onProblemCreated     :: String -> CaideIO ()   -- ^ Run after `caide problem`
    , onProblemCodeCreated :: String -> CaideIO ()   -- ^ Run after `caide lang`
    , onProblemCheckedOut  :: String -> CaideIO ()   -- ^ Run after `caide checkout`
    }

{------------------ Implementation ---------------------}


data ConfigInMemory = ConfigInMemory
    { configParser :: C.ConfigParser
    , modified     :: Bool
    }

data CaideState = CaideState
    { root  :: F.FilePath
    , files :: Map F.FilePath ConfigInMemory
    }


-- Convert deprecated ErrorT style to ExceptT
convertToCaideIO :: IO (Either C.CPError a) -> CaideIO a
convertToCaideIO act = liftIO act >>= convertToCaide

-- Convert deprecated ErrorT style to ExceptT
convertToCaide :: Monad m => Either C.CPError a -> CaideM m a
convertToCaide (Left err) = throwError err
convertToCaide (Right a)  = return a

readConfigFile :: F.FilePath -> CaideIO C.ConfigParser
readConfigFile filePath = convertToCaideIO $ C.readfile C.emptyCP (F.encodeString filePath)

modifyWithFiles :: Monad m =>
                   (Map F.FilePath ConfigInMemory -> Map F.FilePath ConfigInMemory)
                 -> CaideM m ()
modifyWithFiles f = modify' $ \s -> s{files = f (files s)}

writeConfigParser :: F.FilePath -> ConfigInMemory -> IO ()
writeConfigParser file cp = do
    F.createTree $ F.directory file
    F.writeTextFile file . pack . C.to_string $ configParser cp

