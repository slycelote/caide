{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module Caide.Parsers.CodeChef(
      problemParser
    , chelperProblemParser
) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Either (fromRight, rights, )
import qualified Data.List as List
import Data.Maybe (fromMaybe, )
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
import qualified Text.Parsec as Parsec
import Text.Parsec.Text (Parser)

import Network.HTTP.Util (downloadDocument)
import Network.URI (parseURI, uriPath)

import Text.HTML.TagSoup (Tag(..), innerText, fromTagText, parseTags,
                         isTagClose, isTagCloseName, sections, )

import Caide.Parsers.Common (URL, ProblemParser(..), CHelperProblemParser(..),
    isHostOneOf, mergeTextTags, normalizeTestCases, replaceBr)
import Caide.Types
import Caide.Util (tshow)
import Text.HTML.TagSoup.Utils ((~==), (~/=), matching)


problemParser :: ProblemParser
problemParser = ProblemParser
    { problemUrlMatches = isCodeChefUrl
    , parseProblem = doParse
    }

chelperProblemParser :: CHelperProblemParser
chelperProblemParser = CHelperProblemParser
    { chelperId = "codechef"
    , chelperUrlMatches = isCodeChefUrl
    , chelperParse = doParseFromHtml
    }

isCodeChefUrl :: URL -> Bool
isCodeChefUrl = isHostOneOf ["codechef.com", "www.codechef.com"]

orElse :: Either e a -> Either e a -> Either e a
orElse (Right a) _ = Right a
orElse (Left _) b = b

makeTestCase :: Text -> Text -> Either Text TestCase
makeTestCase input' output' =
    if (T.null input' || T.null output')
    then Left "Empty test input and/or output"
    else Right $ TestCase input' output'

parseMarkdown2 :: Text -> Either Text [TestCase]
parseMarkdown2 body' = do
    let suffixes = drop 1 $ T.splitOn "###Sample Input" body'
        parseChunk :: Text -> Text
        parseChunk text = T.strip $ T.dropWhile (not . (`elem` ['\r', '\n'])) text

        parseSuffix :: Text -> Either Text TestCase
        parseSuffix suffix = do
            let chunks = take 2 $ T.splitOn "###" suffix
            when (length chunks /= 2) $
                throwError "Couldn't parse test output"
            let [input', output'] = map parseChunk chunks
            makeTestCase input' output'

        parsedSuffixes = map parseSuffix suffixes
        testCases = rights parsedSuffixes

    return testCases

parseTestCasesFromMarkdown :: Text -> Either Text [TestCase]
parseTestCasesFromMarkdown body' = do
    let chunks = map T.strip $ T.splitOn "```" body'
        n = length chunks
        inputIdx = [1, 5..n-1]
        outputIdx = [3, 7..n-1]
        idxPairs = zip inputIdx outputIdx
        mkTestCase (ix, ox) = TestCase
            { testCaseInput  = chunks!!ix
            , testCaseOutput = chunks!!ox
            }
        testCases1 = map mkTestCase idxPairs
        testCases2 = fromRight [] $ parseMarkdown2 body'

    case List.find (not . null) [testCases1, testCases2] of
        Just testCases -> return testCases
        _ -> throwError "Couldn't find tests"


parseHtml1 :: [Tag Text] -> Either Text TestCase
parseHtml1 [] = Left ""
parseHtml1 tags = do
    let pres = take 2 $ sections (~== "<pre>") tags
    when (length pres /= 2) $ throwError "Can't find <pre> tags"
    let pres' = map (takeWhile (~/= "</pre>")) pres
        [input', output'] = map (T.strip . innerText) pres'
    makeTestCase input' output'


parseHtml2 :: [Tag Text] -> Either Text TestCase
parseHtml2 [] = Left ""
parseHtml2 tags = do
    let ps = take 2 $ sections (~== "<p>") tags
    when (length ps /= 2) $ throwError "Can't find <p> tags"
    let ps' = map (takeWhile (~/= "</p>")) ps
        [input', output'] = map (T.strip . innerText) ps'
    makeTestCase input' output'


parseTestCasesFromHtml :: [Tag Text] -> Either Text [TestCase]
parseTestCasesFromHtml tags = do
    let candidates1 = sections (matching (TagOpen "" [("id", "exampleinput")])) tags
        candidates2 = sections (matching (TagOpen "" [("id", "sampleinput")])) tags
        parsedCandidates1 = map parseHtml1 candidates1
        parsedCandidates2 = map parseHtml2 candidates2
        testCases1 = rights parsedCandidates1
        testCases2 = rights parsedCandidates2

        pres = sections (~== "<pre>") tags
        testsContainer = drop 1 . takeWhile (~/= "</pre>") . dropWhile (~/= "<pre>") . last $ pres
        rootTextNodes = extractCurrentLevelTextNodes . mergeTextTags . replaceBr $ testsContainer
        inputsAndOutputs = filter (not . T.null) . map (T.strip . fromTagText) $ rootTextNodes
        testCases3 = if null pres then [] else [
            TestCase (inputsAndOutputs!!i) (inputsAndOutputs!!(i+1)) | i <- [0, 2 .. length inputsAndOutputs-2]]

    case List.find (not . null) [testCases1, testCases2, testCases3] of
        Just testCases -> return testCases
        _ -> throwError "Couldn't find tests"

data ChefTestCase = ChefTestCase
                  { input :: Text
                  , output :: Text
                  } deriving (Generic, Show)
instance Aeson.FromJSON ChefTestCase

data ChefProblemComponents = ChefProblemComponents
                       { sampleTestCases :: [ChefTestCase]
                       } deriving (Generic, Show)
instance Aeson.FromJSON ChefProblemComponents

data ChefProblem = ChefProblem
                 { problem_code :: Maybe Text
                 , problem_name :: Maybe Text
                 , body :: Maybe Text
                 , problemComponents :: Maybe ChefProblemComponents
                 } deriving (Generic, Show)

instance Aeson.FromJSON ChefProblem


mapLeft :: (e1 -> e2) -> Either e1 a -> Either e2 a
mapLeft f = either (Left . f) Right

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right


parseTestCasesFromJson :: ChefProblem -> Either Text [TestCase]
parseTestCasesFromJson ChefProblem{..} = do
    components <- maybeToEither "no problemComponents" problemComponents
    when (null (sampleTestCases components)) $ throwError "No test cases in JSON"
    let mkTestCase :: ChefTestCase -> TestCase
        mkTestCase ChefTestCase{..} = TestCase input output
    return $ map mkTestCase $ sampleTestCases components


parseFromJson :: Text -> Text -> Either Text (Problem, [TestCase])
parseFromJson problemCode jsonText = do
    chefProblem <- mapLeft T.pack . Aeson.eitherDecodeStrict . encodeUtf8 $ jsonText
    let probName = fromMaybe problemCode $ problem_name chefProblem <|> problem_code chefProblem
        eitherBody = maybeToEither "No body found in JSON" (body chefProblem)
    testCases <- parseTestCasesFromJson chefProblem `orElse`
                 (parseTestCasesFromMarkdown =<< eitherBody) `orElse`
                 ((parseTestCasesFromHtml . parseTags) =<< eitherBody)
    when (null testCases) $ throwError "Could not parse test cases"
    let probType = Stream StdIn StdOut
    return (makeProblem probName problemCode probType, normalizeTestCases testCases)


problemUrlParser :: Parser (T.Text, T.Text)
problemUrlParser = do
    _ <- char '/'
    mbContest <- (pure Nothing              <* string "problems/") <|>
                 ((Just <$> many1 alphaNum) <* string "/problems/")
    probId <- many1 alphaNum
    Parsec.eof
    return (T.pack $ fromMaybe "PRACTICE" mbContest, T.pack probId)
  where
    alphaNum = Parsec.alphaNum
    char = Parsec.char
    many1 = Parsec.many1
    string = Parsec.string

doParseFromHtml :: Text -> IO (Either Text (Problem, [TestCase]))
doParseFromHtml cont = pure $ do
    let tags = parseTags cont
        probType = Stream StdIn StdOut

        problemCode = dropWhile (not . matching (TagOpen "" [("id", "problem-code")])) tags
        spanContents = takeWhile (not . isTagClose) problemCode
        probId = T.strip $ innerText spanContents

        titleTag = dropWhile (~/= "<title>") tags
        titleContents = takeWhile (not . isTagClose) titleTag
        title = T.strip . T.takeWhile (/= '|') $ innerText titleContents

    testCases <- parseTestCasesFromHtml tags
    return (makeProblem title probId probType, normalizeTestCases testCases)

doParse :: URL -> Maybe T.Text -> IO (Either Text (Problem, [TestCase]))
doParse url _ = case urlPath of
    Nothing -> return $ Left $ T.append "Unsupported URL: " url
    Just path -> case Parsec.parse problemUrlParser "" path of
        Left err -> return $ Left $ tshow err
        Right (contestId, probId) -> do
            let apiUrl = T.concat ["https://www.codechef.com/api/contests/", contestId, "/problems/", probId]
            mbDoc <- downloadDocument apiUrl
            return (mbDoc >>= parseFromJson probId)
  where
    urlPath = T.pack . uriPath <$> parseURI (T.unpack url)

extractCurrentLevelTextNodes :: Eq a => [Tag a] -> [Tag a]
extractCurrentLevelTextNodes tags = go tags []
  where
    go [] nodes = reverse nodes
    go (TagText s:rest) nodes = go rest (TagText s:nodes)
    go (TagOpen name _ : rest) nodes = go (dropWhile (not . isTagCloseName name) rest) nodes
    go (_:rest) nodes = go rest nodes

