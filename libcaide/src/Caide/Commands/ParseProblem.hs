module Caide.Commands.ParseProblem(
      cmd
) where

import Control.Monad (forM_)
import Control.Monad.State (liftIO)
import Data.Char (isAlphaNum, isAscii)
import Data.List (find)
import qualified Data.Text as T

import Filesystem (createDirectory, createTree, writeTextFile)
import Filesystem.Path.CurrentOS (decodeString, (</>))

import Caide.Types
import Caide.Codeforces.Parser (codeforcesParser)
import Caide.Configuration (getDefaultLanguage, setActiveProblem, writeProblemConf, writeProblemState)
import Caide.Commands.BuildScaffold (generateScaffoldSolution)
import Caide.Commands.Make (updateTests)


cmd :: CommandHandler
cmd = CommandHandler
    { command = "problem"
    , description = "Parse problem description or create a new problem"
    , usage = "caide problem <URL or problem ID>"
    , action = parseProblem
    }

parsers :: [ProblemParser]
parsers = [codeforcesParser]

parseProblem :: [String] -> CaideIO ()
parseProblem [url] = do
    probId <- case find (`matches` T.pack url) parsers of
        Just parser -> parseExistingProblem url parser
        Nothing     -> createNewProblem url
    root <- caideRoot
    let testDir = root </> decodeString probId </> decodeString ".caideproblem" </> decodeString "test"

    liftIO $ createTree testDir

    _ <- writeProblemState probId
    _ <- writeProblemConf probId

    lang <- getDefaultLanguage
    updateTests
    generateScaffoldSolution [lang]

parseProblem _ = throw $ "Usage: " ++ usage cmd


createNewProblem :: ProblemID -> CaideIO String
createNewProblem probId =
    if any (\c -> not (isAscii c) || not (isAlphaNum c)) probId
    then throw $ probId ++ " is not recognized as a supported URL. " ++
        "To create an empty problem, input a valid problem ID (a string of alphanumeric characters)"
    else do
        root <- caideRoot
        let problemDir = root </> decodeString probId

        -- Prepare problem directory
        liftIO $ createDirectory False problemDir

        -- Set active problem
        setActiveProblem probId
        liftIO $ putStrLn $ "Problem successfully created in folder " ++ probId
        return probId


parseExistingProblem :: String -> ProblemParser -> CaideIO String
parseExistingProblem url parser = do
    parseResult <- liftIO $ parser `parse` T.pack url
    case parseResult of
        Left err -> throw $ "Encountered a problem while parsing:\n" ++ err
        Right (problem, samples) -> do
            root <- caideRoot

            let probId = problemId problem
                problemDir = root </> decodeString probId

            liftIO $ do
                -- Prepare problem directory
                createDirectory False problemDir

                -- Write test cases
                forM_ (zip samples [1::Int ..]) $ \(sample, i) -> do
                    let inFile  = problemDir </> decodeString ("case" ++ show i ++ ".in")
                        outFile = problemDir </> decodeString ("case" ++ show i ++ ".out")
                    writeTextFile inFile  $ testCaseInput sample
                    writeTextFile outFile $ testCaseOutput sample

            -- Set active problem
            setActiveProblem probId
            liftIO $ putStrLn $ "Problem successfully parsed into folder " ++ probId

            return probId

