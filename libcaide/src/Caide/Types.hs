module Caide.Types(
    Problem(..),
    ProblemParser,
    TestCase(..),
    URL
) where

import Data.Text(Text)

data TestCase = TestCase
    { testCaseInput  :: String
    , testCaseOutput :: String
    } deriving (Show)

-- | Structure representing a problem
data Problem = Problem
    { problemName :: Text   -- ^ Human readable identifier, used for displaying in GUI
    , problemId   :: String -- ^ ID used for folder names, code generation etc.
    } deriving (Show)

type URL = Text

-- | Downloads problem data
type ProblemParser = URL -> IO (Either String (Problem, [TestCase]))
