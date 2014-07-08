module Caide.Commands.ParseProblem(
      cmd
) where

import Control.Monad (forM_)
import Data.List (find)
import qualified Data.Text as T

import Filesystem (createDirectory, writeTextFile)
import Filesystem.Path.CurrentOS (decodeString, (</>))

import Caide.Types
import Caide.Codeforces.Parser (codeforcesParser)
import Caide.Configuration (setActiveProblem)
import Data.Char (isAlphaNum, isAscii)

cmd :: CommandHandler
cmd = CommandHandler
    { command = "problem"
    , description = "Parse problem description or create a new problem"
    , usage = "caide problem <URL or problem ID>"
    , action = parseProblem
    }

parsers :: [ProblemParser]
parsers = [codeforcesParser]

parseProblem :: CaideEnvironment -> [String] -> IO ()
parseProblem env [url] = do
    let parser = find (`matches` T.pack url) parsers
    case parser of
        Nothing -> do -- Create a new problem
            let probId = url
            if any (\c -> not (isAscii c) || not (isAlphaNum c)) probId
            then putStrLn "Problem ID must be a string of alphanumeric characters"
            else do
                let problemDir = getRootDirectory env </> decodeString probId
                -- Prepare problem directory
                createDirectory False problemDir
                -- Set active problem
                setActiveProblem env probId
                putStrLn $ "Problem successfully created in folder " ++ probId

        Just p  -> do
            parseResult <- p `parse` T.pack url
            case parseResult of
                Left err -> putStrLn $ "Encountered a problem while parsing:\n" ++ err
                Right (problem, samples) -> do
                    let problemDir = getRootDirectory env </> decodeString (problemId problem)

                    -- Prepare problem directory
                    createDirectory False problemDir

                    -- Write test cases
                    forM_ (zip samples [1::Int ..]) $ \(sample, i) -> do
                        let inFile  = problemDir </> decodeString ("case" ++ show i ++ ".in")
                            outFile = problemDir </> decodeString ("case" ++ show i ++ ".out")
                        writeTextFile inFile  $ testCaseInput sample
                        writeTextFile outFile $ testCaseOutput sample

                    -- Set active problem
                    setActiveProblem env $ problemId problem
                    putStrLn $ "Problem successfully parsed into folder " ++ problemId problem

parseProblem _ _ = putStrLn $ "Usage: " ++ usage cmd
