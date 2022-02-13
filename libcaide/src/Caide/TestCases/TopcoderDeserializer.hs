{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Caide.TestCases.TopcoderDeserializer(
      readToken
    , readDouble
    , readQuotedString
    , readMany
    , runParser
    -- Reexport from Attoparsec
    , Parser
) where

import Control.Monad.Fail (fail)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Functor ((<&>))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Util as T

import Data.Attoparsec.ByteString.Char8 (Parser, IResult(Done, Fail, Partial), Result,
    feed, parse, (<?>), eitherResult,
    anyChar, char, double,
    sepBy, skipSpace, takeTill, takeWhile1)


-- | Adds failing offset to error messages and transforms them to Text
runParser :: Parser a -> Text -> Either Text a
runParser parser text =
    let bs = T.encodeUtf8 text

        processResult :: Result a -> Either Text a
        processResult result = case result of
            Done _ r -> Right r
            Fail bsRest _ _ ->
                let Left err = eitherResult result
                    offset = BS.length bs - BS.length bsRest
                in Left $ "Offset " <> T.pack (show offset <> ": " <> err)
            Partial _ -> processResult $ feed result BS.empty

    in processResult $ parse parser bs

isTokenSeparator :: Char -> Bool
isTokenSeparator c = isSpace c || isJust (T.find (==c) "{}[]\",")

readToken :: Parser Text
readToken = skipSpace >> takeWhile1 (not . isTokenSeparator) <&> T.safeDecodeUtf8

readQuotedString :: Parser Text
readQuotedString = do
    _ <- char '"' <?> "opening double quote"
    ret <- takeTill (== '"')
    _ <- char '"' <?> "closing double quote"
    return $ T.safeDecodeUtf8 ret

readDouble :: Parser Double
readDouble = double

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

