{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Caide.TestCases.TopcoderDeserializer(
      readToken
    , readDouble
    , readQuotedString
    , readMany
    , runParser
    , TopcoderParser
) where

import Control.Applicative (Applicative)
import Control.Monad (unless, when)
import Control.Monad.Except (MonadError, Except, runExcept, throwError)
import Control.Monad.State (StateT, MonadState, get, put, modify', runStateT)
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
advance n = modify' (T.drop n)

scanUntil :: (Char -> Bool) -> TopcoderParser Text
scanUntil charPred = do
    text <- get
    let (skipped, rest) = T.break charPred text
    when (T.null rest) $
        throwError $ T.append "Parse error: required character not found at the following position: " skipped
    put rest
    return skipped

consume :: Char -> TopcoderParser ()
consume c = do
    _ <- scanUntil (not . isSpace)
    cur <- peek
    when (cur /= c) $
        throwError $ T.concat ["Expected char", T.singleton c, "but found ", T.singleton cur]
    advance 1

isTokenSeparator :: Char -> Bool
isTokenSeparator c = isSpace c || isJust (T.find (==c) "{}\",")

readToken :: TopcoderParser Text
readToken = scanUntil (not . isSpace) >> scanUntil isTokenSeparator

readQuotedString :: TopcoderParser Text
readQuotedString = do
    consume '"'
    ret <- scanUntil (/= '"')
    advance 1
    return ret

readDouble :: TopcoderParser Double
readDouble = scanUntil (not . isSpace) >> wrapTextReader T.double

readMany :: TopcoderParser a -> TopcoderParser [a]
readMany elemParser = do
    consume '{'
    ret <- go []
    consume '}'
    return . reverse $ ret
  where
    go accum = do
        _ <- scanUntil (not . isSpace)
        c <- peek
        case c of
            '}' -> return accum
            ',' -> do
                when (null accum) $
                    throwError . T.concat $ ["Unexpected character ", T.singleton c]
                advance 1
                currentElement <- elemParser
                go (currentElement:accum)
            _   -> do
                unless (null accum) $
                    throwError . T.concat $ ["Unexpected character ", T.singleton c]
                currentElement <- elemParser
                go (currentElement:accum)

