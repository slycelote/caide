module Caide.Types(
      Problem (..)
    , ProblemID
    , ProblemParser (..)
    , ProgrammingLanguage (..)
    , TestCase (..)
    , URL
    , CommandHandler (..)
    , Builder
    , Feature (..)
) where

import Data.Text (Text)
import qualified Filesystem.Path as F

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
-- programming language. The first argument to all functions is the path to directory
-- where the problem is located.
data ProgrammingLanguage = ProgrammingLanguage
    { generateScaffold    :: F.FilePath -> IO ()
    , generateTestProgram :: F.FilePath -> IO ()
    , inlineCode          :: F.FilePath -> IO ()
    }


-- | Describes caide command requested by the user, such as 'init' or 'test'
data CommandHandler = CommandHandler
    { command     :: String
    , description :: String
    , usage       :: String
    , action      :: F.FilePath -> [String] -> IO ()
    }

-- | Builder is responsible for building the code and running
--   test program
type Builder =  F.FilePath -- ^ Root caide directory
             -> String     -- ^ Problem ID
             -> IO Bool    -- ^ Returns True if the build was successful and test runner didn't crash

-- | A feature is a piece of optional functionality that may be run
--   at certain points, depending on the configuration. A feature doesn't
--   run by itself, but only in response to certain events.
data Feature = Feature
    { onProblemCreated :: F.FilePath -> String -> IO ()     -- ^ Run after `caide problem`
    , onProblemCodeCreated :: F.FilePath -> String -> IO () -- ^ Run after `caide lang`
    }
