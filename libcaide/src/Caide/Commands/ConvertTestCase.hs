{-# LANGUAGE NamedFieldPuns, OverloadedStrings, ScopedTypeVariables #-}
module Caide.Commands.ConvertTestCase(
      convertTestCaseInput
    , convertTestCaseOutput
    , convertTestCase
    , TestCasePartType(..)
    , convertTopcoderParameters -- ^ for tests only
) where

import qualified Control.Exception.Extended as Exc
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Extended (MonadIO, liftIO, void, when)
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
import qualified Data.Aeson.Parser as AttoAeson
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Data.Attoparsec.ByteString.Char8 ((<?>))

import qualified Data.Scientific as Sci

import qualified Filesystem.Path.CurrentOS as FS
import qualified Filesystem as FS

import Filesystem.Util (readTextFile, writeTextFile)

import Caide.GlobalState (readGlobalState, activeProblem, noActiveProblemError)
import Caide.Problem (readProblemInfo)
import qualified Caide.TestCases.TopcoderDeserializer as TC
import Caide.Types


convertTestCaseInput :: FS.FilePath -> FS.FilePath -> Maybe ProblemID -> CaideIO ()
convertTestCaseInput = convertTestCase_ TestCaseInput

convertTestCaseOutput :: FS.FilePath -> FS.FilePath -> Maybe ProblemID -> CaideIO ()
convertTestCaseOutput = convertTestCase_ TestCaseOutput

data TestCasePartType = TestCaseInput | TestCaseOutput

convertTestCase_ :: TestCasePartType -> FS.FilePath -> FS.FilePath -> Maybe ProblemID -> CaideIO ()
convertTestCase_ inputType inputFile outputFile mbProbId = do
    mbProbId' <- maybe (activeProblem <$> readGlobalState) (pure.Just) mbProbId
    probId <- maybe (throw noActiveProblemError) pure mbProbId'
    problem <- readProblemInfo probId
    res <- liftIO $ convertTestCase inputType problem inputFile outputFile
    either throw pure res

convertTestCase :: MonadIO m => TestCasePartType -> Problem -> FS.FilePath -> FS.FilePath -> m (Either Text ())
convertTestCase inputType problem inputFile outputFile = liftIO $ runExceptT $ do
    inputTime <- liftIO $ FS.getModified inputFile
    needUpdate <- liftIO $ Exc.handleIf isDoesNotExistError (\_ -> return True) $ do
        outputTime <- FS.getModified outputFile
        return $ outputTime < inputTime
    when needUpdate $ case problemType problem of
        Stream {} -> liftIO $ FS.copyFile inputFile outputFile
        Topcoder TopcoderProblemDescription{tcSingleMethod} -> do
            res <- case inputType of
                TestCaseInput -> readAndConvertTopcoderParameters (tcParameters tcSingleMethod) inputFile True
                TestCaseOutput -> readAndConvertTopcoderParameters [tcMethod tcSingleMethod] inputFile False
            liftIO $ writeTextFile outputFile res
        LeetCodeMethod _ -> convertJson inputFile outputFile
        LeetCodeClass {} -> convertJson inputFile outputFile

convertJson :: FS.FilePath -> FS.FilePath -> ExceptT Text IO ()
convertJson inputFile outputFile = do
    text <- liftIO $ readTextFile inputFile
    case text of
        Left err -> throwError err
        Right r ->
            let res = convertJsonValues r
            in either throwError (liftIO . writeTextFile outputFile . T.unlines) res

convertJsonValues :: Text -> Either Text [Text]
convertJsonValues text = do
    let parser = AttoAeson.json `Atto.sepBy` Atto.skipSpace
    jsonValues <- TC.runParser parser text
    pure $ concatMap convertJsonValue jsonValues

readAndConvertTopcoderParameters :: [TopcoderValue] -> FS.FilePath -> Bool -> ExceptT Text IO Text
readAndConvertTopcoderParameters values inputFile isTopcoderFormat = do
    text <- liftIO $ readTextFile inputFile
    case text of
        Left err -> throwError err
        Right r ->
            let res = convertTopcoderParameters values isTopcoderFormat r
            in either throwError (return . T.unlines) res

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

