{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Caide.TestCases.TopcoderDeserializer(
      -- Reexport from Attoparsec
      Parser
    , runParser
    , runParserBS
    , jsonParser

    , readToken
    , readDouble
    , readQuotedString
    , readMany
) where

import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Either (fromLeft)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding.Util as T

import qualified Data.Aeson as Aeson
import qualified Data.Scientific as Sci
import qualified Data.Vector as Vec
import Data.Attoparsec.ByteString.Char8 (Parser, IResult(Done, Fail, Partial), Result,
    feed, parse, (<?>), eitherResult,
    anyChar, char, digit, double,
    many1, sepBy, skipSpace, takeTill, takeWhile1)

import Caide.Types (TopcoderType(..), TopcoderValue(..))

-- | Adds failing offset to error messages and transforms them to Text
-- Returns the remaining portion of input and the parsed value.
runParserBS :: Parser a -> BS.ByteString -> Either Text (BS.ByteString, a)
runParserBS parser bs =
    let processResult :: Result a -> Either Text (BS.ByteString, a)
        processResult result = case result of
            Done i r -> Right (i, r)
            Fail bsRest _ e ->
                let err = fromLeft e $ eitherResult result
                    offset = BS.length bs - BS.length bsRest
                in Left $ "Offset " <> T.pack (show offset <> ": " <> err)
            Partial _ -> processResult $ feed result BS.empty

    in processResult $ parse parser bs

-- | Adds failing offset to error messages and transforms them to Text
runParser :: Parser a -> Text -> Either Text a
runParser parser text = snd <$> runParserBS parser (T.encodeUtf8 text)

isTokenSeparator :: Char -> Bool
isTokenSeparator c = isSpace c || isJust (T.find (==c) "{}[]\",")

readToken :: Parser Text
readToken = skipSpace >> takeWhile1 (not . isTokenSeparator) <&> T.decodeUtf8Lenient

readQuotedString :: Parser Text
readQuotedString = do
    _ <- char '"' <?> "opening double quote"
    ret <- takeTill (== '"')
    _ <- char '"' <?> "closing double quote"
    return $ T.decodeUtf8Lenient ret

readDouble :: Parser Double
readDouble = double

readLong :: Parser Int64
readLong = read <$> many1 digit

readBool :: Parser Bool
readBool = do
    s <- readToken
    if s == "true"
        then return True
        else if s == "false"
            then return False
            else fail $ "Expected 'true' or 'false', got '" <> T.unpack s <> "'"

readMany :: Parser a -> Parser [a]
readMany elemParser = do
    skipSpace
    open <- anyChar <?> "open bracket"
    close <- case open of
        '{' -> pure '}'
        '[' -> pure ']'
        _   -> fail $ "Expected { or [, but found " <> [open]
    skipSpace
    ret <- (elemParser <?> "Array element") `sepBy` (skipSpace >> char ',' >> skipSpace <?> "comma")
    skipSpace
    _ <- char close <?> "close bracket"
    return ret

type JsonConverter a = a -> Aeson.Value

jsonParser :: TopcoderValue -> Parser Aeson.Value
jsonParser TopcoderValue{tcValueName, tcValueType, tcValueDimension} =
    case tcValueDimension of
        0 -> case tcValueType of
            TCInt    -> eval i
            TCLong   -> eval i
            TCDouble -> eval d
            TCString -> eval s
            TCBool   -> eval b
            TCVoid   -> fail "Unsupported type void"
            TypeName tn -> fail $ "Unsupported type " <> T.unpack tn

        1 -> case tcValueType of
            TCInt    -> eval $ v i
            TCLong   -> eval $ v i
            TCDouble -> eval $ v d
            TCString -> eval $ v s
            TCBool   -> eval $ v b
            TCVoid   -> fail "Unsupported type void"
            TypeName tn -> fail $ "Unsupported type " <> T.unpack tn

        2 -> case tcValueType of
            TCInt    -> eval $ v $ v i
            TCLong   -> eval $ v $ v i
            TCDouble -> eval $ v $ v d
            TCString -> eval $ v $ v s
            TCBool   -> eval $ v $ v b
            TCVoid   -> fail "Unsupported type void"
            TypeName tn -> fail $ "Unsupported type " <> T.unpack tn

        3 -> case tcValueType of
            TCInt    -> eval $ v $ v $ v i
            TCLong   -> eval $ v $ v $ v i
            TCDouble -> eval $ v $ v $ v d
            TCString -> eval $ v $ v $ v s
            TCBool   -> eval $ v $ v $ v b
            TCVoid   -> fail "Unsupported type void"
            TypeName tn -> fail $ "Unsupported type " <> T.unpack tn

        _ -> fail $ T.unpack tcValueName <> ": dimension is too high"
  where
    s = (readQuotedString, Aeson.String)
    d = (readDouble, Aeson.Number . Sci.fromFloatDigits)
    i = (readLong, Aeson.Number . (\n -> Sci.scientific n 0) . toInteger)
    b = (readBool, Aeson.Bool)

    v :: (Parser a, JsonConverter a) -> (Parser [a], JsonConverter [a])
    v (parser, converter) = (readMany parser, Aeson.Array . Vec.map converter . Vec.fromList)

    eval :: (Parser a, JsonConverter a) -> Parser Aeson.Value
    eval (parser, converter) = converter <$> parser

