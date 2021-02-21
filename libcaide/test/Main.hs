{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.HUnit

import Caide.TestCases.TopcoderDeserializer (readMany, readToken, runParser)
import Caide.TestCases.Types (deserializeTestReport, makeTestRunResult,
    ComparisonResult(Error, EtalonUnknown, Failed))

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
  , deserializeTestReport "case1 error " ~?=
      [("case1", makeTestRunResult $ Error "")]
  ]

allTests :: Test
allTests = TestList
  [ topcoderDeserializerTests
  , testCaseSerializationTests
  ]

main :: IO ()
main = runTestTTAndExit allTests

