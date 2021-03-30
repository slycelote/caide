{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module ProblemParsers(
      problemParserTests
) where

import Test.HUnit
import qualified Test.HUnit as HUnit
import qualified Data.Text as T

import Caide.Parsers.Common (URL, ProblemParser(..), makeProblemParser)
import qualified Caide.Parsers.HackerRank as HackerRank

import Caide.Types (Problem(problemName, problemId), ProblemType(Stream),
    InputSource(StdIn), OutputTarget(StdOut))
import qualified Caide.Types as Caide

assertParses :: ProblemParser -> URL -> Problem -> [Caide.TestCase] -> Test
assertParses parser url expectedProblem expectedTestCases = HUnit.TestCase $ do
    let ProblemParser{..} = parser
    assertBool "The parser must be able to handle the URL" $ problemUrlMatches url
    parseResult <- parseProblem url Nothing
    case parseResult of
        Left err -> assertFailure $ T.unpack err
        Right (problem, testCases) -> do
            assertEqual "Problem name" (problemName expectedProblem) (problemName problem)
            assertEqual "Problem ID" (problemId expectedProblem) (problemId problem)
            assertEqual "Test cases" expectedTestCases testCases

problemParserTests :: Test
problemParserTests = TestList
    [ hr "https://hackerrank.com/contests/hourrank-18/challenges/wheres-the-marble"
        (makeProblem "Where's the Marble?" "wheres-the-marble")
        [ Caide.TestCase "5 3\n2 5\n7 10\n2 9" "9" ]
    , hr "https://www.hackerrank.com/contests/projecteuler/challenges/euler254/problem"
        (makeProblem "Project Euler #254: Sums of Digit Factorials" "euler254")
        [ Caide.TestCase "2\n3 1000000\n20 1000000" "8\n156" ]
    -- , hr "https://hackerrank.com/challenges/equal"
    --     (makeProblem "Equal" "equal")
    --     [ Caide.TestCase "1\n4\n2 2 3 7" "2" ]
    ]
  where
    hr = assertParses $ makeProblemParser HackerRank.isSupportedUrl HackerRank.htmlParser
    makeProblem name probId = Caide.makeProblem name probId (Stream StdIn StdOut)

