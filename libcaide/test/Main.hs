{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.HUnit

import Caide.TestCases.TopcoderDeserializer (readMany, readToken, runParser)

topcoderDeserializerTests :: Test
topcoderDeserializerTests = TestList
  [ runParser (readMany readToken) "{a, bc,ghij}" ~?= Right ["a", "bc", "ghij"]
  ]

allTests :: Test
allTests = TestList
  [ topcoderDeserializerTests
  ]

main :: IO ()
main = runTestTTAndExit allTests

