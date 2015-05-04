{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Caide.Types(
      Problem (..)
    , ProblemID
    , ProblemType (..)
    , InputSource (..)
    , OutputTarget (..)

    , CaideIO
    , CaideM
    , runCaideM
    , runInDirectory

    , throw
    , assert

    , Option (..)
    , Config
    , ConfigFileHandle
    , readConf
    , createConf
    , flushConf
    , getProp
    , setProp
    , caideRoot

    , HtmlParser (..)
    , ProblemParser (..)
    , ContestParser (..)
    , ProgrammingLanguage (..)
    , TestCase (..)
    , URL
    , Builder
    , BuilderResult(..)
    , Feature (..)
    , noOpFeature
) where

import Control.Applicative (Applicative)
import Control.Monad (forM_, unless, when)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.State (StateT, MonadState, runStateT, gets, modify')
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Char (toLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.ConfigFile as C

import qualified Filesystem as F
import qualified Filesystem.Path as F
import qualified Filesystem.Path.CurrentOS as F

import Text.Read (readMaybe)

import Filesystem.Util (readTextFile, writeTextFile)

data TestCase = TestCase
    { testCaseInput  :: !Text
    , testCaseOutput :: !Text
    } deriving (Show)

type ProblemID = Text

data Problem = Problem
    { problemName :: !Text      -- ^ Human readable identifier, used for displaying in GUI
    , problemId   :: !ProblemID -- ^ ID used for folder names, code generation etc.
    , problemType :: !ProblemType
    } deriving (Show)

data ProblemType = Topcoder | Stream !InputSource !OutputTarget
    deriving (Show)
data InputSource = StdIn | FileInput !F.FilePath
    deriving (Show)
data OutputTarget = StdOut | FileOutput !F.FilePath
    deriving (Show)

type URL = Text

-- | Downloads problem data
data ProblemParser = ProblemParser
    { problemUrlMatches :: URL -> Bool
    , parseProblem      :: URL -> IO (Either Text (Problem, [TestCase]))
    }

data HtmlParser = HtmlParser
    { chelperId            :: Text
    , htmlParserUrlMatches :: URL -> Bool
    , parseFromHtml        :: Text -> Either Text (Problem, [TestCase])
    }

data ContestParser = ContestParser
    { contestUrlMatches :: URL -> Bool
    , parseContest      :: URL -> CaideIO ()
    }


-- | The type encapsulating functions required to support particular target
--   programming language.
data ProgrammingLanguage = ProgrammingLanguage
    { generateScaffold   :: ProblemID -> CaideIO ()
    , inlineCode         :: ProblemID -> CaideIO ()
    }


class Option a where
    optionToString :: a -> String
    optionFromString :: String -> Maybe a

type Config = C.ConfigParser

newtype Monad m => CaideM m a = CaideM { unCaideM :: StateT CaideState (ExceptT C.CPError m) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadError C.CPError, MonadState CaideState)

type CaideIO a = CaideM IO a

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

-- | File handle through which it's possible to access options. See 'setProp', 'getProp'
newtype ConfigFileHandle = FileHandle F.FilePath

runCaideM :: Monad m => CaideM m a -> CaideState -> m (Either C.CPError (a, CaideState))
runCaideM caideAction p = runExceptT $ runStateT (unCaideM caideAction) p

runInDirectory :: F.FilePath -> CaideIO a -> IO (Either C.CPError a)
runInDirectory dir caideAction = do
    let initialState = CaideState { root = dir, files = M.empty }
    ret <- runCaideM caideAction initialState
    case ret of
        Left e -> return $ Left e
        Right (a, finalState) -> do
            forM_ [f | f <- M.assocs (files finalState), modified (snd f)] $
                uncurry writeConfigParser
            return $ Right a

throw :: Monad m => Text -> CaideM m a
throw desc = throwError (C.OtherProblem $ unpack desc, "")

assert :: Monad m => Bool -> Text -> CaideM m ()
assert condition message = unless condition $ throw message

-- | Return root caide directory
caideRoot :: Monad m => CaideM m F.FilePath
caideRoot = gets root

-- | Creates a new config file. Throws if it already exists.
createConf :: Monad m => F.FilePath -> C.ConfigParser -> CaideM m ConfigFileHandle
createConf filePath cp = do
    fileMap <- gets files
    when (M.member filePath fileMap) $
        throw $ T.concat ["File ", toText filePath, " already exists"]
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
        Nothing   -> throw $ T.concat ["File ", toText filePath, " doesn't exist"]
        Just conf -> do
            liftIO $ writeConfigParser filePath conf
            modifyWithFiles $ M.adjust (\c -> c{modified=False}) filePath


getProp :: (Monad m, Option a) => ConfigFileHandle -> String -> String -> CaideM m a
getProp (FileHandle path) section key = do
    mf <- gets (M.lookup path . files)
    case mf of
        Nothing -> throw $ T.concat ["Unknown file handle ", toText path]
        Just f  -> do
            r <- caideRoot
            opt <- convertToCaide $ C.get (extend r $ configParser f) (map toLower section) key
            case optionFromString opt of
                Just a  -> return a
                Nothing -> throw $ T.concat ["Couldn't parse option ", pack opt]

setProp :: (Monad m, Option a) => ConfigFileHandle -> String -> String -> a -> CaideM m ()
setProp (FileHandle path) section key value = do
    mf <- gets (M.lookup path . files)
    case mf of
        Nothing -> throw $ T.append "Unknown file handle " $ toText path
        Just f  -> do
            newConf <- convertToCaide $ C.set (configParser f) section key (optionToString value)
            let newFile = ConfigInMemory { configParser = newConf, modified = True}
            modifyWithFiles $ M.insert path newFile


{------------------ Internals ---------------------}

extend :: F.FilePath -> C.ConfigParser -> C.ConfigParser
extend r conf = conf' { C.accessfunc = C.interpolatingAccess 10, C.usedefault = True }
    where Right conf' = C.set conf "DEFAULT" "caideRoot" $ F.encodeString r

instance Option Bool where
    optionToString False = "no"
    optionToString True  = "yes"

    optionFromString s
        | s' `elem` ["yes", "true", "enabled", "on", "1"]   = Just True
        | s' `elem` ["no", "false", "disabled", "off", "0"] = Just False
        | otherwise = Nothing
      where s' = map toLower s

instance Option Text where
    optionToString = unpack
    optionFromString = Just . pack

instance Option Double where
    optionToString = show
    optionFromString = readMaybe

instance Option [Text] where
    optionToString = unpack . T.intercalate ","
    optionFromString = Just . map T.strip . T.splitOn "," . pack

instance Option ProblemType where
    optionToString Topcoder = "topcoder"
    optionToString (Stream input output) =
        "file," ++ inputSourceToString input ++ "," ++ outputTargetToString output

    optionFromString s
        | map toLower s == "topcoder" = Just Topcoder
    optionFromString s = case optionFromString s of
        Just [probType, inputSource, outputSource]
            | T.map toLower probType == "file" -> Just $ Stream (parseInput inputSource) (parseOutput outputSource)
        _ -> Nothing


data ConfigInMemory = ConfigInMemory
    { configParser :: C.ConfigParser
    , modified     :: Bool
    }

data CaideState = CaideState
    { root  :: F.FilePath
    , files :: Map F.FilePath ConfigInMemory
    }


-- Convert deprecated ErrorT style to ExceptT
convertToCaide :: Monad m => Either C.CPError a -> CaideM m a
convertToCaide (Left err) = throwError err
convertToCaide (Right a)  = return a

readConfigFile :: F.FilePath -> CaideIO C.ConfigParser
readConfigFile filePath = do
    fileReadResult <- liftIO $ readTextFile filePath
    case fileReadResult of
        Left err   -> throw err
        Right text -> convertToCaide $ C.readstring C.emptyCP $ T.unpack text

modifyWithFiles :: Monad m =>
                   (Map F.FilePath ConfigInMemory -> Map F.FilePath ConfigInMemory)
                 -> CaideM m ()
modifyWithFiles f = modify' $ \s -> s{files = f (files s)}

writeConfigParser :: F.FilePath -> ConfigInMemory -> IO ()
writeConfigParser file cp = do
    F.createTree $ F.directory file
    writeTextFile file . pack . C.to_string $ configParser cp

toText :: F.FilePath -> Text
toText path = case F.toText path of
    Left  s -> s
    Right s -> s

inputSourceToString :: InputSource -> String
inputSourceToString StdIn = "stdin"
inputSourceToString (FileInput f) = F.encodeString f

outputTargetToString :: OutputTarget -> String
outputTargetToString StdOut = "stdout"
outputTargetToString (FileOutput f) = F.encodeString f

parseInput :: Text -> InputSource
parseInput "stdin" = StdIn
parseInput f = FileInput . F.fromText $ f

parseOutput :: Text -> OutputTarget
parseOutput "stdout" = StdOut
parseOutput f = FileOutput . F.fromText $ f


