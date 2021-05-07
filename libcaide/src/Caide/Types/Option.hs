{-# LANGUAGE OverloadedStrings #-}
module Caide.Types.Option(
      Option (..)
) where

import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (catMaybes, isNothing)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Text.Read (readMaybe)

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

