module Caide.Types(
    Problem(..),
    ProblemParser,
    TestCase(..),
    URL
) where

import Data.ByteString(ByteString)
import Data.Text(Text)

data TestCase = TestCase
    { testCaseInput  :: ByteString
    , testCaseOutput :: ByteString
    }

-- | Structure representing a problem
data Problem = Problem
    { problemName :: Text       -- ^ Human readable identifier, used for displaying in GUI
    , problemId   :: ByteString -- ^ ID used for folder names, code generation etc.
    }

type URL = Text

-- | Downloads problem data
type ProblemParser = URL -> IO (Either String (Problem, [TestCase]))
