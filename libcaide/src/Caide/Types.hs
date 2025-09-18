{-# LANGUAGE OverloadedStrings #-}

module Caide.Types(
      Problem (..)
    , ProblemID
    , ProblemType (..)
    , InputSource (..)
    , OutputTarget (..)
    , TopcoderType (..)
    , TopcoderValue (..)
    , TopcoderMethod (..)
    , TopcoderProblemDescription (..)
    , defaultLeetCodeClassName
    , makeProblem

    , TestCase (..)
) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

import qualified Filesystem.Path.CurrentOS as FS

data TestCase = TestCase
    { testCaseInput  :: !Text
    , testCaseOutput :: !(Maybe Text)
    } deriving (Show, Eq)

type ProblemID = Text

data Problem = Problem
    { problemName :: !Text      -- ^ Human readable identifier, used for displaying in GUI
    , problemId   :: !ProblemID -- ^ ID used for folder names, code generation etc.
    , problemFloatTolerance :: !Double -- ^ Comparing floating-point values up to this tolerance
    , problemType :: !ProblemType
    , problemCodeSnippets :: !(Map.Map Text Text) -- maps language id to code snippet
    } deriving (Show)

data ProblemType = Topcoder !TopcoderProblemDescription
                 | LeetCodeMethod !TopcoderMethod -- ^ Class with a single method; class name is Solution
                 | LeetCodeClass !Text ![TopcoderValue] ![TopcoderMethod] -- ^ Class name, ctor parameters and methods
                 | Stream !InputSource !OutputTarget
                 deriving (Show, Eq)

data InputSource = StdIn | FileInput !FS.FilePath | InputFilePattern !Text
    deriving (Show, Eq)

data OutputTarget = StdOut | FileOutput !FS.FilePath
    deriving (Show, Eq)

data TopcoderProblemDescription = TopcoderProblemDescription
    { tcClassName        :: !Text
    , tcSingleMethod     :: !TopcoderMethod
    } deriving (Show, Eq)

data TopcoderValue = TopcoderValue
    { tcValueName      :: !Text          -- ^ Parameter/method name
    , tcValueType      :: !TopcoderType  -- ^ Base type of the parameter, e.g. int for vector<vector<int>>
    , tcValueDimension :: !Int           -- ^ Dimension, e.g. 2 for vector<vector<int>>
    } deriving (Show, Eq)

data TopcoderType = TCInt | TCLong | TCDouble | TCString | TCBool | TCVoid
                  | TypeName Text
    deriving (Show, Eq)

data TopcoderMethod = TopcoderMethod
    { tcMethod     :: !TopcoderValue    -- ^ name and return type of the method
    , tcParameters :: ![TopcoderValue]  -- ^ names and types of parameters
    } deriving (Show, Eq)

defaultLeetCodeClassName :: Text
defaultLeetCodeClassName = "Solution"

makeProblem :: Text -> ProblemID -> ProblemType -> Problem
makeProblem name probId probType = Problem
    { problemName = name
    , problemId = probId
    , problemType = probType
    , problemFloatTolerance = 0.000001
    , problemCodeSnippets = Map.empty
    }
