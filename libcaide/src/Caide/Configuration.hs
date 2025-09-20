{-# LANGUAGE CPP, FlexibleContexts, OrPatterns, OverloadedStrings #-}
module Caide.Configuration(
      getProp
    , getPropOptional
    , getPropOrDefault
    , setProp
    , putProp
    , readConfigFile
    , writeConfigFile
) where

import Prelude hiding (readFile, FilePath)

import Control.Monad.Extended (MonadError, liftEither, throwError)
import qualified Control.Monad.State as State
import Data.Char (toLower)
import Data.Either (fromRight)
import Data.Either.Util (mapLeft, maybeToEither)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Filesystem.Util (readTextFile, writeTextFile)

import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FS
import Filesystem.Path.CurrentOS ((</>))

import Data.ConfigFile as CF

import Caide.Types.Option


extend :: FS.FilePath -> CF.ConfigParser -> CF.ConfigParser
extend caideRoot conf = case CF.set conf "DEFAULT" "caideRoot" $ FS.encodeString caideRoot of
    Right conf' -> conf'{ CF.accessfunc = CF.interpolatingAccess 10, CF.usedefault = True }
    _ -> error "Impossible happened: DEFAULT section doesn't exist"

undoExtend :: CF.ConfigParser -> CF.ConfigParser
undoExtend cp = fromRight cp $ CF.remove_option cp "DEFAULT" "caideRoot"

describeError :: CF.CPError -> T.Text
describeError (ParseError s, _) = T.pack s
describeError (SectionAlreadyExists section, _) = "Duplicate section " <> T.pack section
describeError (NoSection section, _) = "No section " <> T.pack section
describeError (NoOption option, _) = "No option " <> T.pack option
describeError (OtherProblem s, _) = T.pack s
describeError (InterpolationError s, _) = T.pack s


getProp :: Option a => CF.ConfigParser -> String -> String -> Either T.Text a
getProp cp section key = do
    opt <- mapLeft describeError $ CF.get cp (map toLower section) key
    let errorMessage = T.concat ["Couldn't parse option ", T.pack section, "/", T.pack key, ": ", T.pack opt]
    maybeToEither errorMessage $ optionFromString opt

getPropOptional :: Option a => CF.ConfigParser -> String -> String -> Either T.Text (Maybe a)
getPropOptional cp section key = do
    let opt = CF.get cp (map toLower section) key
    case opt of
        (Left (CF.NoSection _, _) ; Left (CF.NoOption _, _)) -> Right Nothing
        Right str ->
            let errorMessage = T.concat ["Couldn't parse option ", T.pack section, "/", T.pack key, ": ", T.pack str]
            in maybe (Left errorMessage) (Right . Just) $ optionFromString str
        Left otherErr -> Left $ describeError otherErr

getPropOrDefault :: Option a => CF.ConfigParser -> String -> String -> a -> Either T.Text a
getPropOrDefault cp section key def = do
    mbRes <- getPropOptional cp section key
    pure $ fromMaybe def mbRes

setProp :: Option a => String -> String -> a -> CF.ConfigParser -> Either T.Text CF.ConfigParser
setProp section key value cp = do
    let escapedPercent = concatMap (\c -> if c == '%' then "%%" else [c]) (optionToString value)
    let errorMessage = T.concat ["Couldn't set option ", T.pack section, "/", T.pack key]
    mapLeft (\e -> errorMessage <> ": " <> describeError e) $
        CF.set cp (map toLower section) key escapedPercent

putProp ::
    (Option a, State.MonadState CF.ConfigParser m, MonadError T.Text m) =>
    String -> String -> a -> m ()
putProp section key value = do
    cp <- State.get
    cp' <- case CF.add_section cp section of
        Left (SectionAlreadyExists _, _) -> pure cp
        Left e -> throwError $ describeError e
        Right c -> pure c
    cp'' <- liftEither $ setProp section key value cp'
    State.put cp''

readConfigFile :: FS.FilePath -> FS.FilePath -> IO (Either T.Text CF.ConfigParser)
readConfigFile caideRoot relPath = do
    fileReadResult <- readTextFile $ caideRoot </> relPath
    pure $ case fileReadResult of
        Left e -> Left e
        Right s -> case CF.readstring CF.emptyCP (T.unpack s) of
            Left err -> Left $ describeError err
            Right cp -> Right $ extend caideRoot cp

writeConfigFile :: CF.ConfigParser -> FS.FilePath -> IO ()
writeConfigFile cp filePath = do
    FS.createTree $ FS.directory filePath
    writeTextFile filePath $ T.pack $ CF.to_string $ undoExtend cp

