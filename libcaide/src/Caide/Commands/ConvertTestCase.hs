{-# LANGUAGE NamedFieldPuns, OverloadedStrings, ScopedTypeVariables #-}
module Caide.Commands.ConvertTestCase(
      convertTestCaseInput
    , convertTestCaseOutput
    , convertTopcoderParameters -- ^ for tests only
) where

import qualified Control.Exception.Extended as Exc
import Control.Monad (void, when)
import Control.Monad.Fail (fail)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (($>))
import Data.Int (Int64)
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Data.Word (Word64)
import System.IO.Error (isDoesNotExistError)

import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Data.Attoparsec.ByteString.Char8 ((<?>))

import qualified Data.Scientific as Sci

import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem as FS

import Filesystem.Util (readTextFile, writeTextFile)

import Caide.Configuration (getActiveProblem)
import Caide.Problem (readProblemInfo)
import qualified Caide.TestCases.TopcoderDeserializer as TC
import Caide.Types


convertTestCaseInput :: FS.FilePath -> FS.FilePath -> Maybe ProblemID -> CaideIO ()
convertTestCaseInput = convertTestCase Input

convertTestCaseOutput :: FS.FilePath -> FS.FilePath -> Maybe ProblemID -> CaideIO ()
convertTestCaseOutput = convertTestCase Output

data TestCasePartType = Input | Output

convertTestCase :: TestCasePartType -> FS.FilePath -> FS.FilePath -> Maybe ProblemID -> CaideIO ()
convertTestCase inputType inputFile outputFile mbProbId = do
    probId <- case mbProbId of
        Just p  -> pure p
        Nothing -> getActiveProblem
    problem <- readProblemInfo probId
    inputTime <- liftIO $ FS.getModified inputFile
    needUpdate <- liftIO $ Exc.handleIf isDoesNotExistError (\_ -> return True) $ do
        outputTime <- FS.getModified outputFile
        return $ outputTime < inputTime
    when needUpdate $ case problemType problem of
        Stream _ _ -> liftIO $ FS.copyFile inputFile outputFile
        Topcoder TopcoderProblemDescription{tcSingleMethod} -> do
            res <- case inputType of
                Input -> readAndConvertTopcoderParameters (tcParameters tcSingleMethod) inputFile True
                Output -> readAndConvertTopcoderParameters [tcMethod tcSingleMethod] inputFile False
            liftIO $ writeTextFile outputFile res
        LeetCodeMethod _ -> do
            res <- convertJson inputFile
            liftIO $ writeTextFile outputFile res


convertJson :: FS.FilePath -> CaideIO Text
convertJson inputFile = do
    text <- liftIO $ readTextFile inputFile
    case text of
        Left err -> throw err
        Right r ->
            let res = convertJsonValues r
            in either throw (return . T.unlines) res

convertJsonValues :: Text -> Either Text [Text]
convertJsonValues text = do
    let parser = Aeson.json `Atto.sepBy` Atto.skipSpace
    jsonValues <- TC.runParser parser text
    pure $ concatMap convertJsonValue jsonValues

readAndConvertTopcoderParameters :: [TopcoderValue] -> FS.FilePath -> Bool -> CaideIO Text
readAndConvertTopcoderParameters values inputFile isTopcoderFormat = do
    text <- liftIO $ readTextFile inputFile
    case text of
        Left err -> throw err
        Right r ->
            let res = convertTopcoderParameters values isTopcoderFormat r
            in either throw (return . T.unlines) res

listSepBy :: Monad m => [m a] -> m () -> m [a]
listSepBy parsers sep = do
    let s = List.intersperse (sep $> Nothing) (map (Just <$>) parsers)
    mbRes <- sequence s
    return $ catMaybes mbRes

valuesAsJsonParser :: [TopcoderValue] -> Bool -> Atto.Parser [Aeson.Value]
valuesAsJsonParser values isTopcoderFormat = do
    let (skipSpace, char) = (Atto.skipSpace, Atto.char)
    -- Topcoder plugin currently writes inputs separated by commas and
    -- wrapped in {}. TODO: fix it.
    when isTopcoderFormat $ do
        skipSpace
        void (char '{') <?> "opening brace"
    skipSpace
    let separator = if isTopcoderFormat then skipSpace >> char ',' >> skipSpace else skipSpace
    res <- map TC.jsonParser values `listSepBy` separator
    when isTopcoderFormat $ do
        skipSpace
        void (char '}') <?> "closing brace"
    pure res

convertTopcoderParameters :: [TopcoderValue] -> Bool -> Text -> Either Text [Text]
convertTopcoderParameters values isTopcoderFormat text = do
    let parser = valuesAsJsonParser values isTopcoderFormat
    jsonValues <- TC.runParser parser text
    pure $ concatMap convertJsonValue jsonValues

convertJsonValue :: Aeson.Value -> [Text]
convertJsonValue (Aeson.Array vec) =
    T.pack (show $ Vec.length vec) : concatMap convertJsonValue (Vec.toList vec)
convertJsonValue (Aeson.String s) = [s]
convertJsonValue (Aeson.Number n) = case Sci.toBoundedInteger n of
  Just (i :: Int64) -> [T.pack $ show i]
  _ -> case Sci.toBoundedInteger n of
    Just (i :: Word64) -> [T.pack $ show i]
    _ -> [T.pack $ show n]

convertJsonValue (Aeson.Bool b) = [T.toLower $ T.pack $ show b]
convertJsonValue Aeson.Null = ["null"]
convertJsonValue x = error $ "convertJsonValue: unexpected " <> show x

