{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.HUnit

import Caide.TestCases.TopcoderDeserializer (readMany, readToken, runParser)
import Caide.TestCases.Types (deserializeTestReport,
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
  , deserializeTestReport "case1 error Expected 1 line(s)" ~?= [("case1", Error "Expected 1 line(s)")]
  , deserializeTestReport "case2 unknown" ~?= [("case2", EtalonUnknown)]
  , deserializeTestReport "case1 failed Expected    1    line(s)" ~?= [("case1", Failed "Expected    1    line(s)")]
  , deserializeTestReport "case1 error " ~?= [("case1", Error "")]
  ]

allTests :: Test
allTests = TestList
  [ topcoderDeserializerTests
  , testCaseSerializationTests
  ]

main :: IO ()
main = runTestTTAndExit allTests

