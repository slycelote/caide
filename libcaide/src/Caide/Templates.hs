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
import Filesystem.Util (pathToText, readTextFile, writeTextFile)

import Caide.Logger (logWarn)
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
        Right current -> do
            mbOriginal <- liftIO $ mbReadFile originalPath
            case (mbOriginal == Right current, mbOriginal == Right builtin) of
                (True, True)   -> return builtin -- No modification
                (True, False)  -> overwrite      -- Only upstream modified
                (False, True)  -> return current -- Only user modified
                (False, False) -> case current == builtin of
                    True -> overwrite -- user and upstream modified in consistent way
                    False -> do
                        logWarn $ "Builtin template " <> pathToText path <>
                                  " was updated both upstream and locally. Compilation error is possible." <>
                                  " (Rename or delete local copy to accept upstream changes.)"
                        return current

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

