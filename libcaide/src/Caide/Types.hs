module Caide.Types(
      Problem (..)
    , ProblemID
    , ProblemParser (..)
    , ProgrammingLanguage (..)
    , TestCase (..)
    , URL
    , CommandHandler (..)
    , Builder
) where

import Data.Text (Text)
import qualified Filesystem.Path as F

data TestCase = TestCase
    { testCaseInput  :: Text
    , testCaseOutput :: Text
    } deriving (Show)

type ProblemID = String

-- | Structure representing a problem
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
             -> IO Bool    -- ^ Returns True if the build was successful
