{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when)
import System.Exit (exitFailure)

import Caide.TestCases.TopcoderDeserializer (TopcoderParser, readMany, readToken, runParser)

main :: IO ()
main = do
    let v = runParser (readMany readToken) "{a, bc,ghij}"
    when (v /= Right ["a", "bc", "ghij"]) $ exitFailure

