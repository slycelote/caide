{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Main where

import Test.HUnit
import System.Environment (getArgs)

import qualified Data.Aeson as Aeson

import ProblemParsers (problemParserTests)

import Caide.MustacheUtil (enrich)
import Caide.TestCases.TopcoderDeserializer (readMany, readToken, runParser)
import Caide.TestCases.Types (deserializeTestReport, humanReadableReport,
    TestRunResult(testRunTime), makeTestRunResult,
    ComparisonResult(Error, EtalonUnknown, Failed, Success))

topcoderDeserializerTests :: Test
topcoderDeserializerTests = TestList
  [ runParser (readMany readToken) "{a, bc,ghij}" ~?= Right ["a", "bc", "ghij"]
  ]


testCaseSerializationTests :: Test
testCaseSerializationTests = TestList
  [ deserializeTestReport "" ~?= []
  , deserializeTestReport "  " ~?= []
  , deserializeTestReport " \r\n " ~?= []
  , deserializeTestReport "case1 error Expected 1 line(s)" ~?=
      [("case1", makeTestRunResult $ Error "Expected 1 line(s)")]
  , deserializeTestReport "case2 unknown" ~?=
      [("case2", makeTestRunResult EtalonUnknown)]
  , deserializeTestReport "case1 failed Expected    1    line(s)" ~?=
      [("case1", makeTestRunResult $ Failed "Expected    1    line(s)")]
  , deserializeTestReport "case1 error " ~?= [("case1", makeTestRunResult $ Error "")]
  , deserializeTestReport "case1 #foo:bar failed Error message" ~?=
      [("case1", makeTestRunResult $ Failed "Error message")]
  , deserializeTestReport "case1 #time:12ms OK" ~?=
      [("case1", (makeTestRunResult Success){testRunTime = Just 0.012})]
  , deserializeTestReport "case1 #time:11 OK" ~?=
      [("case1", (makeTestRunResult Success){testRunTime = Just 0.011})]
  , deserializeTestReport "case1 #time:10 #foo:bar #time:9ms OK" ~?=
      [("case1", (makeTestRunResult Success){testRunTime = Just 0.009})]
  , deserializeTestReport "case1 #time:8 failed #time:7ms error" ~?=
      [("case1", (makeTestRunResult $ Failed "#time:7ms error"){testRunTime = Just 0.008})]

  , humanReadableReport [] ~?= ""
  , humanReadableReport [ ("case1", makeTestRunResult $ Failed "error")
                        , ("case2", (makeTestRunResult Success){testRunTime = Just 0.012})
                        ] ~?=
        "case1  FAILED: error\ncase2      OK (12ms)"
  , humanReadableReport [("case1", (makeTestRunResult $ Error "message"){testRunTime = Just 0.005})] ~?=
        "case1   ERROR (5ms): message"
  ]

enrichTests :: Test
enrichTests = TestList
  [ enrich (o [("foo", n 1)]) ~?=
      o [("foo", n 1), ("foo_is_1", b True)]
  , enrich (a [n 1, n 2, n 3]) ~?= a [n 1, n 2, n 3]
  , enrich (a [o [("foo", s "#")]]) ~?=
      a [o [("foo", s "#"), ("isfirst", b True), ("islast", b True)]]
  ] where
  o = Aeson.Object
  n = Aeson.Number
  b = Aeson.Bool
  a = Aeson.Array
  s = Aeson.String


allTests :: Test
allTests = TestList
  [ topcoderDeserializerTests
  , testCaseSerializationTests
  , TestLabel "enrich" enrichTests
  , TestLabel "live-parsers" problemParserTests
  ]

filterTests :: [String] -> Test -> Test
filterTests labels t@(TestLabel label innerTest) =
    if label `elem` labels
        then t
        else TestLabel label (filterTests labels innerTest)
filterTests labels (TestList list) = TestList (map (filterTests labels) list)
filterTests _ _ = TestList []

main :: IO ()
main = do
    labels <- getArgs
    let tests = if null labels then allTests else filterTests labels allTests
    runTestTTAndExit tests

