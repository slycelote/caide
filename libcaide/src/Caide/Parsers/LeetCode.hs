{-# LANGUAGE DeriveGeneric, NamedFieldPuns, OverloadedStrings #-}
module Caide.Parsers.LeetCode(
      problemParser
) where

import Prelude hiding (lines)
import Control.Applicative ((<|>))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Bifunctor (bimap)
import Data.Either (rights)
import Data.Either.Util (maybeToEither, mapLeft)
import Data.Function ((&), on)
import Data.Functor (($>))
import qualified Data.List as List
import Data.List.Util (chunksOf)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.Util (safeDecodeUtf8)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON, (.:))

import Data.Attoparsec.ByteString.Char8 (letter_ascii, char, digit,
    option, skipSpace, skipMany1, sepBy, (<?>), parseOnly)

import Network.URI (parseURI, pathSegments, URI(uriPath, uriQuery, uriFragment))
import Network.HTTP.Types.Header (hAcceptLanguage, hContentType, hReferer)

import Text.HTML.TagSoup (Tag(..), innerText, parseTags,
                         isTagCloseName, isTagOpenName, partitions, sections, )

import qualified Text.Parsec as Parsec

import qualified Caide.HttpClient as Http
import Caide.Parsers.Common (URL, ProblemParser(..), isHostOneOf, normalizeTestCases)
import Caide.Types
import Caide.Util (tshow)


isLeetCodeUrl :: URL -> Bool
isLeetCodeUrl = isHostOneOf ["leetcode.com", "www.leetcode.com", "leetcode-cn.com", "www.leetcode-cn.com"]

problemParser :: ProblemParser
problemParser = ProblemParser
    { problemUrlMatches = isLeetCodeUrl
    , parseProblem = doParse
    }


doParse :: Http.Client -> URL -> Maybe T.Text -> IO (Either Text (Problem, [TestCase]))
doParse client url _ = case (mbUri, mbPathSegments) of
    (Just uri, Just seg) | length seg >= 2 && seg !! (length seg - 2) == "problems" -> do
        let probId = last seg
            apiUri = uri{uriPath = "/graphql", uriQuery="", uriFragment=""}
            apiQuery = "{\"operationName\":\"questionData\",\"variables\":{\"titleSlug\":\"" <> LBS8.pack probId <> "\"}, " <>
                       "\"query\":\"query questionData($titleSlug: String!) {  question(titleSlug: $titleSlug) { " <>
                       "title    titleSlug    content    exampleTestcases " <>
                       "codeSnippets {      lang      langSlug      code      } " <>
                       "sampleTestCase    metaData    } " <>
                       "}\"}"
        mbJson <- Http.post client apiUri apiQuery [
            (hContentType, "application/json"),
            (hReferer, BS8.pack (show uri)),
            (hAcceptLanguage, "en-US,en;q=0.5")]
        pure (mbJson >>= parseFromGraphQL (T.pack probId))

    _ -> pure $ Left $ "Unsupported URL: " <> url
  where
    mbUri = parseURI (T.unpack url)
    mbPathSegments = pathSegments <$> mbUri

data CodeSnippet = CodeSnippet
                   { lang :: !Text
                   , langSlug :: !Text
                   , code :: !Text
                   } deriving (Show, Generic)
instance FromJSON CodeSnippet


data Param = Param
             { name :: !Text
             , type_ :: !Text
             } deriving (Show, Generic)

instance FromJSON Param where
    parseJSON = Aeson.genericParseJSON (Aeson.defaultOptions{Aeson.fieldLabelModifier = modField})
        where modField "type_" = "type"
              modField s = s


data MethodMetaData = MethodMetaData
                { params :: ![Param]
                , desc :: !Param
                } deriving (Show, Generic)

instance FromJSON MethodMetaData where
    parseJSON = Aeson.withObject "metadata" $ \md -> do
        returnVal <- md .: "return"
        flip (Aeson.withObject "return") returnVal $ \ret -> do
            returnDesc <- Param <$> (md .: "name") <*> (ret .: "type")
            MethodMetaData <$> (md .: "params") <*> pure returnDesc


data ConstructorMetaData = ConstructorMetaData
                         { ctorParams :: ![Param] -- renamed to avoid name clashing
                         } deriving (Show, Generic)

instance FromJSON ConstructorMetaData where
    parseJSON = Aeson.genericParseJSON (Aeson.defaultOptions{Aeson.fieldLabelModifier = modField})
        where modField "ctorParams" = "params"
              modField s = s


data ClassMetaData = ClassMetaData
                   { classname :: !Text
                   , constructor :: !(Maybe ConstructorMetaData)
                   , methods :: ![MethodMetaData]
                   , systemdesign :: !(Maybe Bool)
                   } deriving (Show, Generic)
instance FromJSON ClassMetaData


data MetaData = CM ClassMetaData
              | MM MethodMetaData
              deriving (Show, Generic)

instance FromJSON MetaData where
    parseJSON = Aeson.withEmbeddedJSON "metadata string" $ \mdJson -> flip (Aeson.withObject "metadata json") mdJson $ \o ->
        if HashMap.member "classname" o
            then CM <$> Aeson.parseJSON (Aeson.Object o)
            else MM <$> Aeson.parseJSON (Aeson.Object o)


data Question = Question
                { title :: !(Maybe Text)
                -- for Chinese site
                , translatedTitle :: !(Maybe Text)
                , titleSlug :: !(Maybe Text)
                , content :: !(Maybe Text)
                -- note the spelling
                , exampleTestcases :: !(Maybe Text)
                , codeSnippets :: !(Maybe [CodeSnippet])
                , sampleTestCase :: !(Maybe Text)
                , metaData :: !MetaData
                } deriving (Show, Generic)

instance FromJSON Question where
    parseJSON = Aeson.withObject "Question" $ \top -> do
        data_ <- top .: "data"
        flip (Aeson.withObject "data") data_ $ \d -> do
            question <- d .: "question"
            Aeson.genericParseJSON Aeson.defaultOptions question


isWhiteSpace :: Text -> Bool
isWhiteSpace t = T.null (T.strip t)

-- Compare by number of tests with known outputs first, then by total number.
numTests :: [TestCase] -> (Int, Int)
numTests tests = (length [t | t@(TestCase _ (Just _)) <- tests], length tests)

typeParser :: Parsec.Parsec Text () (TopcoderType, Int)
typeParser =
    let (try, string, many, eof, letter, alphaNum) =
            (Parsec.try, Parsec.string, Parsec.many, Parsec.eof, Parsec.letter, Parsec.alphaNum)

        identifierParser :: Parsec.Parsec Text () Text
        identifierParser = do
            first <- letter <|> Parsec.char '_'
            rest <- many (alphaNum <|> Parsec.char '_')
            return $ T.pack $ first:rest

        elementTypeParser = try (string "integer" $> TCInt)
            <|> try (string "string" $> TCString)
            <|> try (string "double" $> TCDouble)
            <|> try (string "boolean" $> TCBool)
            <|> try (string "void" $> TCVoid)
            -- identifier must always be last
            <|> TypeName <$> identifierParser

        format1 = do
            _ <- try $ string "list<"
            arr <- many (string "list<")
            let n = 1 + length arr
            typ <- elementTypeParser
            _ <- Parsec.count n (string ">")
            eof
            return (typ, n)

        format2 = do
            typ <- elementTypeParser
            arr <- many (string "[]")
            eof
            return (typ, length arr)

    in format1 <|> format2

parseValue :: Param -> Either Text TopcoderValue
parseValue Param{name, type_} = do
    (typ, dim) <- mapLeft tshow $ Parsec.parse typeParser (T.unpack name) type_
    return TopcoderValue{tcValueName = name, tcValueType = typ, tcValueDimension = dim}

parseConstructor :: Maybe ConstructorMetaData -> Either Text [TopcoderValue]
parseConstructor Nothing = pure []
parseConstructor (Just ConstructorMetaData{ctorParams}) = mapM parseValue ctorParams

parseMethod :: MethodMetaData -> Either Text TopcoderMethod
parseMethod MethodMetaData{params, desc} = TopcoderMethod <$> parseValue desc <*> mapM parseValue params

{-# ANN parseClass ("HLint: ignore Redundant <$>" :: String) #-}
parseClass :: ClassMetaData -> Either Text ProblemType
parseClass ClassMetaData{classname, constructor, methods} =
    LeetCodeClass <$> pure classname <*> parseConstructor constructor <*> mapM parseMethod methods

{-# ANN parseFromGraphQL ("HLint: ignore Use maybe" :: String) #-}
parseFromGraphQL :: ProblemID -> LBS8.ByteString -> Either Text (Problem, [TestCase])
parseFromGraphQL probId responseBody = do
    question <- mapLeft T.pack $ Aeson.eitherDecode responseBody
    probType <- case metaData question of
        CM clazz  -> parseClass clazz
        MM method -> LeetCodeMethod <$> parseMethod method

    let probName = fromMaybe probId $ translatedTitle question <|> title question
        snippets = codeSnippets question & fromMaybe [] & map (\q -> (langSlug q, code q)) & Map.fromList
        problem = (makeProblem probName (fromMaybe probId (titleSlug question)) probType)
            { problemCodeSnippets = snippets}

        paramsPerTest = case probType of
            LeetCodeMethod singleMethod -> length $ tcParameters singleMethod
            LeetCodeClass {} -> 2 -- list of method names, and list of lists of arguments
            Topcoder _ -> error "Impossible happened"
            Stream {} -> error "Impossible happened"

        testParsings = [ fromContent (content question)
                       , fromExamples (exampleTestcases question) paramsPerTest
                       , fromSample (sampleTestCase question)
                       ]
        validTestParsings = rights testParsings
        bestParsing = if null validTestParsings
            then []
            else List.maximumBy (compare `on` numTests) validTestParsings

    return (problem, bestParsing)

fromSample :: Maybe Text -> Either Text [TestCase]
fromSample Nothing = Left "sampleTestCase not provided"
fromSample (Just t) | isWhiteSpace t = Left "sampleTestCase not provided"
fromSample (Just t) = Right [TestCase t Nothing]

fromExamples :: Maybe Text -> Int -> Either Text [TestCase]
fromExamples Nothing _ = Left "exampleTestcases not provided"
-- Split by lines, each line is a value of a single parameter in a single test case
fromExamples (Just t) numLinesPerTestCase = let
    lines = t & T.lines & filter (not . isWhiteSpace)
    tests = lines & chunksOf numLinesPerTestCase & map T.unlines &
            map (\input -> TestCase input Nothing)

    in case () of
      _ | numLinesPerTestCase <= 0  -> Left $
            "Can't parse exampleTestcases: invalid number of method params " <> tshow numLinesPerTestCase
        | length lines `mod` numLinesPerTestCase /= 0 -> Left $
            "Can't parse exampleTestcases: number of method params is " <> tshow numLinesPerTestCase <>
            ", number of input lines is " <> tshow (length lines)
        | otherwise -> Right tests

parseInput :: Text -> Either Text Text
parseInput t = let
    identParser = skipMany1 (digit <|> letter_ascii <|> char '_') <?> "identifier"
    paramParser = (skipSpace >>
                   option () (identParser >> skipSpace >> char '=' >> skipSpace) >>
                   Aeson.json) <?> "parameter"
    parser = paramParser `sepBy` (skipSpace >> option () (char ',' >> skipSpace))
    result = parseOnly parser (T.encodeUtf8 t)

    valuesToText :: [Aeson.Value] -> Text
    valuesToText vals = vals & map Aeson.encode & map LBS8.toStrict & map safeDecodeUtf8 & T.unlines

    in bimap T.pack valuesToText result

{-# ANN fromContent ("HLint: ignore Use bimap" :: String) #-}
fromContent :: Maybe Text -> Either Text [TestCase]
fromContent Nothing = Left "content not provided"
fromContent (Just html) = let
    pre = parseTags html & sections (isTagOpenName "pre") & map (takeWhile (not . isTagCloseName "pre"))
    testCases = map extractFromPre pre

    extractFromPre :: [Tag Text] -> Either Text TestCase
    extractFromPre tags = do
        let textWithBoldTitles = tags & partitions (isTagOpenName "strong") &
                map (break (isTagCloseName "strong")) &
                map (\(first, rest) -> (innerText first, innerText rest))
            findText s = listToMaybe [ text | (bold, text) <- textWithBoldTitles, s `T.isInfixOf` bold ]
        input <- maybeToEither "Couldn't find input" $ findText "Input"
        let output = findText "Output"
        parsedInput <- parseInput input
        return $ TestCase parsedInput output

    in Right $ normalizeTestCases $ rights testCases

