{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Caide.Templates(
      getTemplate
    , copyTemplateUnlessExists
    , templates
) where

import Control.Monad.Except (liftIO)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Encoding.Util (universalNewlineConversionOnInput)
import Filesystem (isFile, createTree)
import qualified Filesystem.Path.CurrentOS as F
import Filesystem.Path.CurrentOS ((</>))

import Data.FileEmbed (embedDir)
import Filesystem.Util (readTextFile, writeTextFile)

import Caide.Types

templates' :: [(String, ByteString)]
templates' = $(embedDir "res/templates")

templates :: [(F.FilePath, Text)]
templates = [(F.decodeString fp, universalNewlineConversionOnInput $ decodeUtf8 cont) | (fp, cont) <- templates']

getTemplate :: F.FilePath -> CaideIO Text
getTemplate path = do
    root <- caideRoot
    let mbBuiltin = lookup path templates
        Just builtin = mbBuiltin
    when (isNothing mbBuiltin) $
        throw "Internal error: unexpected template file requested"
    let currentPath = root </> "templates" </> path
        originalPath = root </> ".caide" </> "templates" </> path
        overwrite = do
            liftIO $ do
                createTree $ root </> "templates"
                createTree $ root </> ".caide" </> "templates"
                writeTextFile currentPath builtin
                writeTextFile originalPath builtin
            return builtin

    mbCurrent <- liftIO $ mbReadFile currentPath
    case mbCurrent of
        Left _ -> overwrite
        Right current -> if current == builtin
            then return builtin
            else do
                mbOriginal <- liftIO $ mbReadFile originalPath
                case mbOriginal of
                    Left _ -> overwrite
                    Right original -> if original == current
                        then overwrite
                        else return current

mbReadFile :: F.FilePath -> IO (Either Text Text)
mbReadFile path = do
    exist <- isFile path
    if exist then readTextFile path else return $ Left "File doesn't exist"

copyTemplateUnlessExists :: F.FilePath -> F.FilePath -> CaideIO ()
copyTemplateUnlessExists templateName to = do
    fileExists <- liftIO $ isFile to
    unless fileExists $ do
        cont <- getTemplate templateName
        liftIO $ writeTextFile to cont

