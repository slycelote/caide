{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.HUnit

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
  , humanReadableReport [ ("case1", (makeTestRunResult $ Error "message"){testRunTime = Just 0.005}) ] ~?=
        "case1   ERROR (5ms): message"
  ]

allTests :: Test
allTests = TestList
  [ topcoderDeserializerTests
  , testCaseSerializationTests
  ]

main :: IO ()
main = runTestTTAndExit allTests

