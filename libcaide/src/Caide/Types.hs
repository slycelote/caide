module Caide.Types(
      Problem (..)
    , ProblemID
    , ProblemParser
    , ProgrammingLanguage (..)
    , TestCase (..)
    , URL
) where

import Data.Text (Text)
import qualified Filesystem.Path as F

data TestCase = TestCase
    { testCaseInput  :: String
    , testCaseOutput :: String
    } deriving (Show)

type ProblemID = String

-- | Structure representing a problem
data Problem = Problem
    { problemName :: Text      -- ^ Human readable identifier, used for displaying in GUI
    , problemId   :: ProblemID -- ^ ID used for folder names, code generation etc.
    } deriving (Show)

type URL = Text

-- | Downloads problem data
type ProblemParser = URL -> IO (Either String (Problem, [TestCase]))


-- | The type encapsulating functions required to support particular target
-- programming language. The first argument to all functions is the path to directory
-- where the problem is located.
data ProgrammingLanguage = ProgrammingLanguage
    { generateScaffold    :: F.FilePath -> IO ()
    , generateTestProgram :: F.FilePath -> IO ()
    , inlineCode          :: F.FilePath -> IO ()
    }
