module Caide.Commands.ParseProblem(
      cmd
    , parseExistingProblem
) where

import Control.Monad (forM_, when)
import Control.Monad.State (liftIO)
import Data.Char (isAlphaNum, isAscii)
import qualified Data.Text as T

import Filesystem (createDirectory, createTree, writeTextFile, isDirectory)
import Filesystem.Path.CurrentOS (decodeString, encodeString, (</>))

import Caide.Types
import Caide.Configuration (getDefaultLanguage, setActiveProblem, writeProblemConf, writeProblemState)
import Caide.Commands.BuildScaffold (generateScaffoldSolution)
import Caide.Commands.Make (updateTests)
import Caide.Registry (findProblemParser)


cmd :: CommandHandler
cmd = CommandHandler
    { command = "problem"
    , description = "Parse problem description or create a new problem"
    , usage = "caide problem <URL or problem ID>"
    , action = doParseProblem
    }


doParseProblem :: [String] -> CaideIO ()
doParseProblem [url] = case findProblemParser (T.pack url) of
    Just parser -> parseExistingProblem (T.pack url) parser
    Nothing     -> createNewProblem url

doParseProblem _ = throw $ "Usage: " ++ usage cmd

initializeProblem :: ProblemID -> CaideIO ()
initializeProblem probId = do
    root <- caideRoot
    let testDir = root </> decodeString probId </> decodeString ".caideproblem" </> decodeString "test"

    liftIO $ createTree testDir

    _ <- writeProblemState probId
    _ <- writeProblemConf probId

    lang <- getDefaultLanguage
    updateTests
    generateScaffoldSolution [lang]


createNewProblem :: ProblemID -> CaideIO ()
createNewProblem probId = do
    when (any (\c -> not (isAscii c) || not (isAlphaNum c)) probId) $
        throw $ probId ++ " is not recognized as a supported URL. " ++
            "To create an empty problem, input a valid problem ID (a string of alphanumeric characters)"

    root <- caideRoot
    let problemDir = root </> decodeString probId

    -- Prepare problem directory
    liftIO $ createDirectory False problemDir

    -- Set active problem
    setActiveProblem probId
    liftIO $ putStrLn $ "Problem successfully created in folder " ++ probId
    initializeProblem probId


parseExistingProblem :: URL -> ProblemParser -> CaideIO ()
parseExistingProblem url parser = do
    parseResult <- liftIO $ parser `parseProblem` url
    case parseResult of
        Left err -> throw $ "Encountered a problem while parsing:\n" ++ err
        Right (problem, samples) -> do
            root <- caideRoot

            let probId = problemId problem
                problemDir = root </> decodeString probId

            problemDirExists <- liftIO $ isDirectory problemDir
            when problemDirExists $
                throw $ "Problem directory already exists: " ++ encodeString problemDir

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

            initializeProblem probId

