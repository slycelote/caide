{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Caide.Types.Option(
      Option (..)
) where

import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (catMaybes, isNothing)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Text.Read (readMaybe)

import qualified Filesystem.Path.CurrentOS as FS

import Caide.Types

-- | Typeclass for values that can be represented as a short(-ish) string,
-- suitable for command-line options.
class Option a where
    optionToString :: a -> String
    optionToText   :: a -> Text
    optionFromString :: String -> Maybe a
    optionFromText   :: Text -> Maybe a

    optionToString = unpack . optionToText
    optionToText   = pack . optionToString
    optionFromString = optionFromText . pack
    optionFromText   = optionFromString . unpack

instance Option Bool where
    optionToString False = "no"
    optionToString True  = "yes"

    optionFromString s
        | s' `elem` ["yes", "true", "enabled", "on", "1"]   = Just True
        | s' `elem` ["no", "false", "disabled", "off", "0"] = Just False
        | otherwise = Nothing
      where s' = map toLower s

instance Option Text where
    optionToText = id
    optionFromText = Just

instance Option Int where
    optionToString = show
    optionFromString = readMaybe

instance Option Double where
    optionToString = show
    optionFromString = readMaybe

instance Option a => Option [a] where
    optionToString = intercalate "," . map optionToString
    optionFromText text = if any isNothing list
                        then Nothing
                        else Just . catMaybes $ list
      where list = map (optionFromText . T.strip) . T.splitOn "," $ text

instance Option TopcoderType where
    optionToText TCInt    = "int"
    optionToText TCLong   = "long"
    optionToText TCDouble = "double"
    optionToText TCString = "String"
    optionToText TCBool   = "bool"
    optionToText TCVoid   = "void"
    optionToText (TypeName s) = s

    optionFromString "int"    = Just TCInt
    optionFromString "long"   = Just TCLong
    optionFromString "double" = Just TCDouble
    optionFromString "String" = Just TCString
    optionFromString "string" = Just TCString
    optionFromString "bool"   = Just TCBool
    optionFromString "void"   = Just TCVoid
    optionFromString s        = Just $ TypeName $ T.pack s

-- name:vvType
instance Option TopcoderValue where
    optionToString p = concat [
        unpack (tcValueName p), ":", replicate (tcValueDimension p) 'v', optionToString (tcValueType p)]

    optionFromText s = do
        [paramName, paramType] <- pure $ T.splitOn ":" s
        (baseType, dimension) <-
            if paramType == "void"
                then pure (TCVoid, 0)
                else do
                    let dim = T.length . T.takeWhile (=='v') $ paramType
                    typ <- optionFromText . T.dropWhile (=='v') $ paramType
                    pure (typ, dim)
        pure TopcoderValue
            { tcValueName = paramName
            , tcValueType = baseType
            , tcValueDimension = dimension
            }


-- method:retType,param1:type1,param2:type2
instance Option TopcoderMethod where
    optionToString TopcoderMethod{tcMethod,tcParameters} = optionToString (tcMethod:tcParameters)

    optionFromString s = case optionFromString s of
        Just (method:params) -> Just $ TopcoderMethod method params
        _ -> Nothing


instance Option ProblemType where
    -- topcoder,class,method:retType,param1:type1,param2:type2
    optionToString (Topcoder desc) =
        "topcoder," <> T.unpack (tcClassName desc) <> "," <> optionToString (tcSingleMethod desc)

    -- leetcode,method:retType,param1:type1,param2:type2
    optionToString (LeetCodeMethod m) = "leetcode," <> optionToString m

    -- leetcode;className,ctorParam1:type1,ctorParam2:type2;method:retType,param1:type1,param2:type2;anotherMethod:retType,param1:type1,param2:type2
    optionToString (LeetCodeClass className ctorParams methods) =
        "leetcode;" <> T.unpack className <> concat [',' : optionToString p | p <- ctorParams]
            <> concat [';' : optionToString method | method <- methods]

    optionToString (Stream input output) =
        "file," <> inputSourceToString input <> "," <> outputTargetToString output

    optionFromText s | "topcoder," `T.isPrefixOf` s = case maybeParams of
        Just (className, (method:params)) -> Just $ Topcoder TopcoderProblemDescription
            { tcClassName = className
            , tcSingleMethod = TopcoderMethod { tcMethod = method, tcParameters = params }
            }
        _ -> Nothing
      where
        components = T.splitOn "," s
        maybeParams = case components of
            (_:className:paramsStr) -> do
                valueDefs <- mapM optionFromText paramsStr
                pure (className, valueDefs)
            _ -> Nothing

    optionFromText s | "leetcode," `T.isPrefixOf` s = let
        components = drop 1 $ T.splitOn "," s
        mbValues = mapM optionFromText components
        in case mbValues of
            Just (method:params) -> Just $ LeetCodeMethod $ TopcoderMethod method params
            _ -> Nothing

    optionFromText s | "leetcode;" `T.isPrefixOf` s = do
        (_:ctorToken:methodTokens) <- pure $ T.splitOn ";" s
        (className:ctorParamsTokens) <- pure $ T.splitOn "," ctorToken
        ctorParams <- mapM optionFromText ctorParamsTokens
        methods <- mapM optionFromText methodTokens
        pure $ LeetCodeClass className ctorParams methods


    optionFromText s = case optionFromText s of
        Just [probType, inputSource, outputSource]
            | probType == "file" -> Just $ Stream (parseInput inputSource) (parseOutput outputSource)
        _ -> Nothing

inputSourceToString :: InputSource -> String
inputSourceToString StdIn = "stdin"
inputSourceToString (FileInput f) = FS.encodeString f
inputSourceToString (InputFilePattern p) = T.unpack $ "/" <> p <> "/"

outputTargetToString :: OutputTarget -> String
outputTargetToString StdOut = "stdout"
outputTargetToString (FileOutput f) = FS.encodeString f

parseInput :: Text -> InputSource
parseInput "stdin" = StdIn
parseInput f
    | T.length f >= 2 && T.head f == '/' && T.last f == '/'
        = InputFilePattern . T.init . T.tail $ f
    | otherwise
        = FileInput . FS.fromText $ f

parseOutput :: Text -> OutputTarget
parseOutput "stdout" = StdOut
parseOutput f = FileOutput . FS.fromText $ f
