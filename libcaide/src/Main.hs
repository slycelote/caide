module Main where

import qualified Data.Text as T

import Caide.Codeforces.Parser (codeforcesParser)
import Caide.Types (problemName)

main :: IO()
main = do
    parseResult <- codeforcesParser $ T.pack "http://codeforces.com/contest/400/problem/B"
    case parseResult of
        Left err -> putStrLn err
        Right (problem, _) -> putStrLn . T.unpack $ problemName problem
