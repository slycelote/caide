{-# LANGUAGE OverloadedStrings #-}
module Caide.Commands.ParseProblem(
      createProblem
    , saveProblemWithScaffold
    , parseProblems
) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Util (whenJust)
import Control.Monad.Except (catchError)
import Control.Monad.State (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum, isAscii)
import Data.Either (lefts)
import Data.Function ((&))
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO.Util as T
import Data.Text (Text)
import Data.Text.Encoding.Util (safeDecodeUtf8)

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map

import Filesystem (createDirectory, createTree, writeTextFile, isDirectory)
import Filesystem.Path.CurrentOS (fromText, decodeString, (</>))
import Filesystem.Util (pathToText)
import qualified Filesystem.Path.CurrentOS as F

import Caide.Types
import Caide.Configuration (setActiveProblem, getProblemConfigFile,
                            getProblemStateFile, defaultProblemConfig, defaultProblemState,
                            describeError)
import Caide.Commands.BuildScaffold (generateScaffoldSolution)
import Caide.Commands.Make (updateTests)
import Caide.Parsers.Common (URL, ProblemParser(parseProblem), CHelperProblemParser(chelperParse))
import Caide.Registry (findCHelperProblemParserByURL, findProblemParser)
import Caide.Settings (defaultLanguage)
import Caide.Util (mapWithLimitedThreads, readTextFile', withLock)



createProblem :: URL -> T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> CaideIO ()
createProblem url problemTypeStr maybeLangStr maybeFilePathStr maybeOverrideId = do
    case (maybeFilePathStr, findProblemParser url, findCHelperProblemParserByURL url) of
        (_, Just parser, _)  -> parseExistingProblem url parser maybeOverrideId maybeFilePathStr
        (Just _, _, Nothing) -> throw $ "File parser for URL " <> url <> " not found"
        (Just filePathStr, _, Just chelperParser) -> parseExistingProblemFromCHelper chelperParser (fromText filePathStr) maybeOverrideId
        (Nothing, Nothing,  _) -> case optionFromText problemTypeStr of
            Nothing    -> throw . T.concat $ ["Incorrect problem type: ", problemTypeStr]
            Just pType -> createNewProblem url pType

    lang <- case maybeLangStr of
        Just langStr -> return langStr
        Nothing -> defaultLanguage <$> caideSettings

    generateScaffoldSolution lang

initializeProblem :: Problem -> CaideIO ()
initializeProblem problem = withLock $ do
    root <- caideRoot
    let probId = problemId problem
        testDir = root </> fromText probId </> ".caideproblem" </> "test"

    setActiveProblem probId
    problemConfPath  <- getProblemConfigFile probId
    problemStatePath <- getProblemStateFile probId

    liftIO $ createTree testDir

    hProblemConf <- createConf problemConfPath defaultProblemConfig
    setProp hProblemConf "problem" "name" $ problemName problem
    setProp hProblemConf "problem" "type" $ problemType problem
    let snippets = problemCodeSnippets problem & Aeson.encode & LBS.toStrict & safeDecodeUtf8
    unless (Map.null (problemCodeSnippets problem)) $
        setProp hProblemConf "problem" "snippets" snippets

    hProblemState <- createConf problemStatePath defaultProblemState

    flushConf hProblemConf
    flushConf hProblemState

    updateTests (Just probId)

isAcceptableCharacter :: Char -> Bool
isAcceptableCharacter c = isAscii c && (c == '_' || c == '-' || isAlphaNum c)

createNewProblem :: ProblemID -> ProblemType -> CaideIO ()
createNewProblem probId probType = do
    when (T.any (not . isAcceptableCharacter) probId) $
        throw . T.concat $ [probId, " is not recognized as a supported URL. ",
            "To create an empty problem, input a valid problem ID (a string of alphanumeric characters)"]

    root <- caideRoot
    let problemDir = root </> fromText probId
        problem = makeProblem probId probId probType

    -- Prepare problem directory
    liftIO $ createDirectory False problemDir

    initializeProblem problem
    liftIO $ T.putStrLn . T.concat $ ["Problem successfully created in folder ", probId]


saveProblem :: Problem -> [TestCase] -> CaideIO ()
saveProblem problem samples = do
    root <- caideRoot

    let probId = problemId problem
        problemDir = root </> fromText probId

    problemDirExists <- liftIO $ isDirectory problemDir
    when problemDirExists $
        throw . T.concat $ ["Problem directory already exists: ", pathToText problemDir]

    liftIO $ do
        -- Prepare problem directory
        createDirectory False problemDir

        -- Write test cases
        forM_ (zip samples [1::Int ..]) $ \(sample, i) -> do
            let inFile  = problemDir </> decodeString ("case" ++ show i ++ ".in")
                outFile = problemDir </> decodeString ("case" ++ show i ++ ".out")
            writeTextFile inFile  $ testCaseInput sample
            whenJust (testCaseOutput sample) $ \o -> writeTextFile outFile $ o

    initializeProblem problem
    liftIO $ T.putStrLn . T.concat $ ["Problem successfully parsed into folder ", probId]

saveProblemWithScaffold :: Problem -> [TestCase] -> CaideIO ()
saveProblemWithScaffold problem samples = do
    saveProblem problem samples
    lang <- defaultLanguage <$> caideSettings
    generateScaffoldSolution lang

overrideProblemId :: Maybe ProblemID -> Problem -> Problem
overrideProblemId Nothing p = p
overrideProblemId (Just probId) p = p { problemId = probId }

parseExistingProblemFromCHelper :: CHelperProblemParser -> F.FilePath -> Maybe ProblemID -> CaideIO ()
parseExistingProblemFromCHelper parser filePath mbId = do
    html <- readTextFile' filePath
    parseResult <- liftIO $ chelperParse parser html
    case parseResult of
        Left err -> throw . T.unlines $ ["Encountered a problem while parsing:", err]
        Right (problem, samples) -> saveProblem (overrideProblemId mbId problem) samples

parseExistingProblem :: URL -> ProblemParser -> Maybe ProblemID -> Maybe Text -> CaideIO ()
parseExistingProblem url parser mbId mbFilePath = do
    mbHtml <- case mbFilePath of
        Nothing -> pure Nothing
        Just path -> Just <$> readTextFile' (decodeString $ T.unpack path)
    parseResult <- liftIO $ parseProblem parser url mbHtml
    case parseResult of
        Left err -> throw . T.unlines $ ["Encountered a problem while parsing:", err]
        Right (problem, samples) -> saveProblem (overrideProblemId mbId problem) samples

-- Pair of URL and either error message or parsed problem
type ParseResult = (URL, Either Text (Problem, [TestCase]))

errorMessage :: ParseResult -> Maybe Text
errorMessage (_, Right _) = Nothing
errorMessage (url, Left err) = Just $ T.concat [url, ": ", err]

trySaveProblemWithScaffold :: ParseResult -> CaideIO (Either Text ())
trySaveProblemWithScaffold pr@(_, Left _) = return $ Left $ fromJust $ errorMessage pr
trySaveProblemWithScaffold (url, Right (problem, tests)) =
    (saveProblemWithScaffold problem tests >> return (Right ())) `catchError` \err ->
        return $ Left $ T.concat [url, ": ", T.pack $ describeError err]


parseProblems :: Int -> [URL] -> CaideIO ()
parseProblems numThreads urls = do
    parseResults <- liftIO $ mapWithLimitedThreads numThreads tryParseProblem urls
    results <- mapM trySaveProblemWithScaffold $ reverse $ zip urls parseResults
    let errors = lefts results
    unless (null errors) $
        throw $ T.unlines ("Some problems failed to parse.": errors)

tryParseProblem :: URL -> IO (Either T.Text (Problem, [TestCase]))
tryParseProblem url = case findProblemParser url of
    Nothing -> return . Left . T.concat $ ["Couldn't find problem parser for URL: ", url]
    Just parser -> parseProblem parser url Nothing

