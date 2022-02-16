{-# LANGUAGE NamedFieldPuns, OverloadedStrings, ScopedTypeVariables #-}
module Caide.Commands.ConvertTestCase(
      convertTestCaseInput
    , convertTopcoderParameters -- ^ for tests only
) where

import qualified Control.Exception.Extended as Exc
import Control.Monad (void, when)
import Control.Monad.Fail (fail)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (($>))
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import System.IO.Error (isDoesNotExistError)

import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Data.Attoparsec.ByteString.Char8 ((<?>))
import qualified Data.Scientific as Sci

import qualified Filesystem.Path.CurrentOS as FS
import Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem as FS

import Filesystem.Util (readTextFile, writeTextFile)

import Caide.Configuration (getActiveProblem)
import Caide.Paths (problemDir, testsDir)
import Caide.Problem (readProblemInfo)
import Caide.TestCases.TopcoderDeserializer (Parser, runParser,
    readDouble, readQuotedString, readToken, readMany)
import Caide.Types


convertTestCaseInput :: FS.FilePath -> Maybe ProblemID -> CaideIO ()
convertTestCaseInput inputFile mbProbId = do
    root <- caideRoot
    probId <- case mbProbId of
        Just p  -> pure p
        Nothing -> getActiveProblem
    problem <- readProblemInfo probId
    let name = FS.filename inputFile
        outputFile = problemDir root probId </> testsDir </> name

    needUpdate <- liftIO $ isOutputOutdated inputFile outputFile
    when needUpdate $ case problemType problem of
        Stream _ _ -> liftIO $ FS.copyFile inputFile outputFile
        Topcoder TopcoderProblemDescription{tcSingleMethod} -> do
            res <- convertTopcoderMethod tcSingleMethod inputFile True
            liftIO $ writeTextFile outputFile res
        LeetCodeMethod tcMethod -> do
            res <- convertTopcoderMethod tcMethod inputFile False
            liftIO $ writeTextFile outputFile res

isOutputOutdated :: FS.FilePath -> FS.FilePath -> IO Bool
isOutputOutdated inputFile outputFile = do
    inputTime <- FS.getModified inputFile
    mbOutputTime <- Exc.catchIf isDoesNotExistError
        (Just <$> FS.getModified outputFile)
        (\_ -> pure Nothing)
    return $ case mbOutputTime of
        Just outputTime -> outputTime < inputTime
        Nothing -> True

convertTopcoderMethod :: TopcoderMethod -> FS.FilePath -> Bool -> CaideIO Text
convertTopcoderMethod TopcoderMethod{tcParameters} = readAndConvertTopcoderParameters tcParameters

readAndConvertTopcoderParameters :: [TopcoderValue] -> FS.FilePath -> Bool -> CaideIO Text
readAndConvertTopcoderParameters values inputFile isTopcoderFormat = do
    text <- liftIO $ readTextFile inputFile
    case text of
        Left err -> throw err
        Right r ->
            let res = convertTopcoderParameters values isTopcoderFormat r
            in either throw (return . T.unlines) res

type JsonConverter a = a -> Aeson.Value

jsonParser :: TopcoderValue -> Parser Aeson.Value
jsonParser TopcoderValue{tcValueName, tcValueType, tcValueDimension} =
    case tcValueDimension of
        0 -> case tcValueType of
            TCDouble -> eval d
            TCString -> eval s
            _        -> eval t

        1 -> case tcValueType of
            TCDouble -> eval $ v d
            TCString -> eval $ v s
            _        -> eval $ v t

        2 -> case tcValueType of
            TCDouble -> eval $ v $ v d
            TCString -> eval $ v $ v s
            _        -> eval $ v $ v t

        3 -> case tcValueType of
            TCDouble -> eval $ v $ v $ v d
            TCString -> eval $ v $ v $ v s
            _        -> eval $ v $ v $ v t

        _ -> fail $ T.unpack tcValueName <> ": dimension is too high"
  where
    s = (readQuotedString, Aeson.String)
    d = (readDouble, Aeson.Number . Sci.fromFloatDigits)
    t = (readToken, Aeson.String)

    v :: (Parser a, JsonConverter a) -> (Parser [a], JsonConverter [a])
    v (parser, converter) = (readMany parser, Aeson.Array . Vec.map converter . Vec.fromList)

    eval :: (Parser a, JsonConverter a) -> Parser Aeson.Value
    eval (parser, converter) = converter <$> parser

listSepBy :: Monad m => [m a] -> m () -> m [a]
listSepBy parsers sep = do
    let s = List.intersperse (sep $> Nothing) (map (Just <$>) parsers)
    mbRes <- sequence s
    return $ catMaybes mbRes

valuesAsJsonParser :: [TopcoderValue] -> Bool -> Parser [Aeson.Value]
valuesAsJsonParser values isTopcoderFormat = do
    let (skipSpace, char) = (Atto.skipSpace, Atto.char)
    -- Topcoder plugin currently writes inputs separated by commas and
    -- wrapped in {}. TODO: fix it.
    when isTopcoderFormat $ do
        skipSpace
        void (char '{') <?> "opening brace"
    skipSpace
    let separator = if isTopcoderFormat then skipSpace >> char ',' >> skipSpace else skipSpace
    res <- map jsonParser values `listSepBy` separator
    when isTopcoderFormat $ do
        skipSpace
        void (char '}') <?> "closing brace"
    pure res

convertTopcoderParameters :: [TopcoderValue] -> Bool -> Text -> Either Text [Text]
convertTopcoderParameters values isTopcoderFormat text = do
    let parser = valuesAsJsonParser values isTopcoderFormat
    jsonValues <- runParser parser text
    pure $ concatMap convertJsonValue jsonValues

convertJsonValue :: Aeson.Value -> [Text]
convertJsonValue (Aeson.Array vec) =
    T.pack (show $ Vec.length vec) : concatMap convertJsonValue (Vec.toList vec)
convertJsonValue (Aeson.String s) = [s]
convertJsonValue (Aeson.Number n) = [T.pack $ show n]
convertJsonValue (Aeson.Bool b) = [T.toLower $ T.pack $ show b]
convertJsonValue x = error $ "convertJsonValue: unexpected " <> show x

