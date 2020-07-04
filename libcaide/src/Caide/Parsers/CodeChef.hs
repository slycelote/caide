{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.CodeChef(
      codeChefParser
    , codeChefHtmlParser
) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Except (throwError)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import qualified Data.Aeson as Aeson
import qualified Text.Parsec as Parsec
import Text.Parsec.Text (Parser)

import Network.HTTP.Util (downloadDocument)
import Network.URI (parseURI, uriAuthority, uriPath, uriRegName)

import Text.HTML.TagSoup (Tag(..), innerText, fromTagText, parseTags,
                         isTagCloseName, sections)

import Caide.Parsers.Common (mergeTextTags, replaceBr)
import Caide.Types
import Caide.Util (tshow)
import Text.HTML.TagSoup.Utils ((~==), (~/=), (~~/==))


codeChefParser :: ProblemParser
codeChefParser = ProblemParser
    { problemUrlMatches = isCodeChefUrl
    , parseProblem = doParse
    }

codeChefHtmlParser :: HtmlParser
codeChefHtmlParser = HtmlParser
    { chelperId = "codechef"
    , htmlParserUrlMatches = isCodeChefUrl
    , parseFromHtml = doParseFromHtml
    }

isCodeChefUrl :: URL -> Bool
isCodeChefUrl url = case parseURI (T.unpack url) >>= uriAuthority of
    Nothing   -> False
    Just auth -> uriRegName auth `elem` ["codechef.com", "www.codechef.com"]

parseTestCasesFromMarkdown :: Text -> Either Text [TestCase]
parseTestCasesFromMarkdown body = do
    let chunks = map T.strip $ T.splitOn "```" body
        n = length chunks
        inputIdx = [1, 5..n-1]
        outputIdx = [3, 7..n-1]
        idxPairs = zip inputIdx outputIdx
        mkTestCase (ix, ox) = TestCase
            { testCaseInput  = chunks!!ix
            , testCaseOutput = chunks!!ox
            }
    when (null idxPairs) $ throwError $ T.concat ["Couldn't find tests ", tshow chunks, tshow idxPairs]
    return $ map mkTestCase idxPairs

parseTestCasesFromHtml :: [Tag Text] -> Either Text [TestCase]
parseTestCasesFromHtml tags = do
    let pres = sections (~== "<pre>") tags
        testsContainer = drop 1 . takeWhile (~/= "</pre>") . dropWhile (~/= "<pre>") . last $ pres
        rootTextNodes = extractCurrentLevelTextNodes . mergeTextTags . replaceBr $ testsContainer
        inputsAndOutputs = filter (not . T.null) . map (T.strip . fromTagText) $ rootTextNodes
        testCases = [TestCase (inputsAndOutputs!!i) (inputsAndOutputs!!(i+1)) |
                        i <- [0, 2 .. length inputsAndOutputs-2]]
    when (null pres || null testCases) $
        throwError $ T.concat ["Couldn't find tests ", tshow rootTextNodes]
    return testCases

fromAesonString :: Aeson.Value -> Maybe Text
fromAesonString value = case value of
    Aeson.String s -> Just s
    _ -> Nothing

orElse :: Either e a -> Either e a -> Either e a
orElse (Right a) _ = Right a
orElse (Left _) b = b

parseFromJson :: Text -> Text -> Either Text (Problem, [TestCase])
parseFromJson problemCode jsonText = do
    let mbJson = Aeson.decode . LBS.fromStrict . encodeUtf8 $ jsonText
    obj <- case mbJson of
        Just (Aeson.Object o) -> return o
        _ -> throwError "Could not parse CodeChef JSON"
    let probName = fromMaybe problemCode $
                     (HashMap.lookup "problem_name" obj >>= fromAesonString) <|>
                     (HashMap.lookup "problem_code" obj >>= fromAesonString)
        mbBody = HashMap.lookup "body" obj
    body <- case mbBody of
        Just (Aeson.String s) -> return s
        _ -> throwError "Could not find problem body in CodeChef JSON"

    testCases <- parseTestCasesFromMarkdown body `orElse`
                 parseTestCasesFromHtml (parseTags body) `orElse`
                 pure []
    let probType = Stream StdIn StdOut
    return (Problem probName problemCode probType, testCases)


problemUrlParser :: Parser (T.Text, T.Text)
problemUrlParser = do
    _ <- char '/'
    mbContest <- (pure Nothing                <* string "problems/") <|>
                 ((Just <$> (many1 alphaNum)) <* string "/problems/")
    probId <- many1 alphaNum
    Parsec.eof
    return (T.pack $ fromMaybe "PRACTICE" mbContest, T.pack probId)
  where
    alphaNum = Parsec.alphaNum
    char = Parsec.char
    many1 = Parsec.many1
    string = Parsec.string

doParseFromHtml :: Text -> Either Text (Problem, [TestCase])
doParseFromHtml cont = do
    let tags = parseTags cont
        probType = Stream StdIn StdOut

        -- problem ID
        problemCode = dropWhile (~/= "<span id=problem-code>") tags
        spanContents = takeWhile (~/= "</span>") problemCode
        -- TODO title
        title = T.strip $ innerText spanContents
        probId = T.append "chef" title

        -- test cases
        problemPage = dropWhile (\t ->
            t ~/= "<div id=problem-page-complete>" && t ~/= "<div id=problem-page>") tags
        content = takeWhile (~/= "</div>") . dropWhile (~~/== "<div class=content>") $ problemPage

    testCases <- parseTestCasesFromHtml content
    return (Problem title probId probType, testCases)

doParse :: URL -> IO (Either Text (Problem, [TestCase]))
doParse url = case urlPath of
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

