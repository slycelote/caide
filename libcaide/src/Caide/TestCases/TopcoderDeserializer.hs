{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Caide.TestCases.TopcoderDeserializer(
      readToken
    , readDouble
    , readQuotedString
    , readMany
    , runParser
    , TopcoderParser
) where

import Control.Monad (unless, when)
import Control.Monad.Except (MonadError, Except, runExcept, throwError)
import Control.Monad.State (StateT, MonadState, get, put, runStateT)
import Data.Char (isSpace)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T

-- | Parser of input/output data in Topcoder format
newtype TopcoderParser a = TopcoderParser { unTP :: StateT Text (Except Text) a }
    deriving (Functor, Applicative, Monad, MonadError Text, MonadState Text)

runParser :: TopcoderParser a -> Text -> Either Text a
runParser parser text = case runExcept (runStateT (unTP parser) text) of
    Left err     -> Left err
    Right (a, _) -> Right a

wrapTextReader :: T.Reader a -> TopcoderParser a
wrapTextReader reader = TopcoderParser { unTP = do
    text <- get
    let res = reader text
    case res of
        Left err -> throwError . T.pack $ err
        Right (a, rest) -> put rest >> return a
}

peek :: TopcoderParser Char
peek = do
    text <- get
    when (T.null text) $ throwError "Unexpected end of file"
    return . T.head $ text

advance :: Int -> TopcoderParser ()
advance n = do
    text <- get
    when (T.length text < n) $ throwError "Unexpected end of file"
    put $ T.drop n text

scanUntil :: (Char -> Bool) -> TopcoderParser Text
scanUntil charPred = do
    text <- get
    let (skipped, rest) = T.break charPred text
    put rest
    return skipped

readChar :: TopcoderParser Char
readChar = do
    _ <- scanUntil (not . isSpace)
    c <- peek
    advance 1
    return c

consume :: Char -> TopcoderParser ()
consume c = do
    cur <- readChar
    when (cur /= c) $
        throwError $ "Expected char " <> T.singleton c <> "but found " <> T.singleton cur

isTokenSeparator :: Char -> Bool
isTokenSeparator c = isSpace c || isJust (T.find (==c) "{}[]\",")

readToken :: TopcoderParser Text
readToken = scanUntil (not . isSpace) >> peek >> scanUntil isTokenSeparator

readQuotedString :: TopcoderParser Text
readQuotedString = do
    consume '"'
    ret <- scanUntil (== '"')
    advance 1
    return ret

readDouble :: TopcoderParser Double
readDouble = scanUntil (not . isSpace) >> wrapTextReader T.double

readMany :: TopcoderParser a -> TopcoderParser [a]
readMany elemParser = do
    open <- readChar
    when (open /= '{' && open /= '[') $
        throwError $ "Expected { or [, but found " <> T.singleton open
    ret <- go []
    close <- readChar
    when (close /= '}' && close /= ']') $
        throwError $ "Expected } or ], but found " <> T.singleton close
    return . reverse $ ret
  where
    go accum = do
        _ <- scanUntil (not . isSpace)
        c <- peek
        case c of
            '}' -> return accum
            ']' -> return accum
            ',' -> do
                when (null accum) $
                    throwError $ "Unexpected character " <> T.singleton c
                advance 1
                currentElement <- elemParser
                go (currentElement:accum)
            _   -> do
                unless (null accum) $
                    throwError $ "Unexpected character " <> T.singleton c
                currentElement <- elemParser
                go (currentElement:accum)

