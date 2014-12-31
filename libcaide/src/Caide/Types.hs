{-# LANGUAGE Rank2Types, FlexibleInstances #-}

module Caide.Types(
      Problem (..)
    , ProblemID
    , Option
    , optionToString
    , CaideProject (..)
    , ProblemParser (..)
    , ProgrammingLanguage (..)
    , TestCase (..)
    , URL
    , CommandHandler (..)
    , Builder
    , BuilderResult(..)
    , Feature (..)
    , CaideEnvironment
) where

import Data.Text (Text)
import qualified Filesystem.Path as F
import qualified Data.ConfigFile as C

data TestCase = TestCase
    { testCaseInput  :: Text
    , testCaseOutput :: Text
    } deriving (Show)

type ProblemID = String

data Problem = Problem
    { problemName :: Text      -- ^ Human readable identifier, used for displaying in GUI
    , problemId   :: ProblemID -- ^ ID used for folder names, code generation etc.
    } deriving (Show)

type URL = Text

-- | Downloads problem data
data ProblemParser = ProblemParser
    { matches :: URL -> Bool
    , parse   :: URL -> IO (Either String (Problem, [TestCase]))
    }


-- | The type encapsulating functions required to support particular target
-- programming language. The second argument to all functions is the path to directory
-- where the problem is located.
data ProgrammingLanguage = ProgrammingLanguage
    { generateScaffold    :: CaideEnvironment -> F.FilePath -> IO ()
    , generateTestProgram :: CaideEnvironment -> F.FilePath -> IO ()
    , inlineCode          :: CaideEnvironment -> F.FilePath -> IO ()
    }


class C.Get_C a => Option a where
    optionToString :: a -> String

instance Option Bool where
    optionToString False = "no"
    optionToString True  = "yes"

instance Option [Char] where
    optionToString = id


data CaideProject = CaideProject
    { getUserOption     :: Option a => String -> String -> IO a
    , getInternalOption :: Option a => String -> String -> IO a
    , setInternalOption :: Option a => String -> String -> a -> IO ()
    , getRootDirectory  :: F.FilePath
    , saveProject       :: IO ()
    }

type CaideEnvironment = CaideProject

-- | Describes caide command requested by the user, such as 'init' or 'test'
data CommandHandler = CommandHandler
    { command      :: String
    , description  :: String
    , usage        :: String
    , action       :: CaideEnvironment -> [String] -> IO (Maybe String)
    }

data BuilderResult = BuildFailed | TestsFailed | TestsNotRun | TestsOK

-- | Builder is responsible for building the code and running
--   test program
type Builder  = CaideEnvironment     -- ^ Caide environment
              -> String              -- ^ Problem ID
              -> IO BuilderResult

-- | A feature is a piece of optional functionality that may be run
--   at certain points, depending on the configuration. A feature doesn't
--   run by itself, but only in response to certain events.
data Feature = Feature
    { onProblemCreated     :: CaideEnvironment -> String -> IO ()   -- ^ Run after `caide problem`
    , onProblemCodeCreated :: CaideEnvironment -> String -> IO ()   -- ^ Run after `caide lang`
    , onProblemCheckedOut  :: CaideEnvironment -> String -> IO ()   -- ^ Run after `caide checkout`
    }

