{-# LANGUAGE CPP, FlexibleContexts, OrPatterns, OverloadedStrings #-}
module Caide.Configuration(
      getProp
    , getPropOrDefault
    , setProp
    , putProp
    , readConfigFile
    , writeConfigFile
    , describeError

    , SystemCompilerInfo(..)
    , defaultCaideConf
    , defaultCaideState
    , defaultProblemConfig
    , defaultProblemState
) where

import Prelude hiding (readFile, FilePath)

import Control.Monad.Extended (MonadError, liftEither)
import qualified Control.Monad.State as State
import Data.Char (toLower)
import Data.Either.Util (mapLeft, maybeToEither)
import Data.List (intercalate, isPrefixOf)
import qualified Data.Text as T
import Filesystem.Util (readTextFile, writeTextFile)
import System.Info (arch, os)

import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FS
import Filesystem.Path (FilePath)
import Filesystem.Path.CurrentOS (encodeString, (</>))

import Data.ConfigFile as CF

import Caide.Types.Option


-- Aliases with a different argument order, for convenience.
addSection :: SectionSpec -> ConfigParser -> Either CPError ConfigParser
addSection section conf = add_section conf section

setValue :: SectionSpec -> OptionSpec -> String -> ConfigParser -> Either CPError ConfigParser
setValue section key value conf = set conf section key value

extend :: FS.FilePath -> CF.ConfigParser -> CF.ConfigParser
extend caideRoot conf = case CF.set conf "DEFAULT" "caideRoot" $ encodeString caideRoot of
    Right conf' -> conf'{ CF.accessfunc = CF.interpolatingAccess 10, CF.usedefault = True }
    _ -> error "Impossible happened: DEFAULT section doesn't exist"

undoExtend :: CF.ConfigParser -> CF.ConfigParser
undoExtend cp = either (const cp) id $ CF.remove_option cp "DEFAULT" "caideRoot"

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

getPropOrDefault :: Option a => CF.ConfigParser -> String -> String -> a -> Either T.Text a
getPropOrDefault cp section key def = do
    let opt = CF.get cp (map toLower section) key
    case opt of
        (Left (CF.NoSection _, _) ; Left (CF.NoOption _, _)) -> pure def
        Right str ->
            let errorMessage = T.concat ["Couldn't parse option ", T.pack section, "/", T.pack key, ": ", T.pack str]
            in maybeToEither errorMessage $ optionFromString str
        Left otherErr -> Left $ describeError otherErr

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
    s <- State.get
    s' <- liftEither $ setProp section key value s
    State.put s'

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

data SystemCompilerInfo = SystemCompilerInfo
    { mscver :: Int
    , gccIncludeDirectories :: [String]
    }

defaultCaideConf :: FilePath -> Bool -> SystemCompilerInfo -> ConfigParser
defaultCaideConf root useSystemHeaders compiler = forceEither $
    addSection "core" emptyCP >>=
    setValue "core" "language" defaultLanguage >>=
    setValue "core" "features" "" >>=
    addSection "cpp" >>=
    setValue "cpp" "keep_macros" "_WIN32,_WIN64,_MSC_VER,__GNUC__,__clang__,__cplusplus,__STDC_VERSION__,__linux,__linux__" >>=
    setValue "cpp" "max_consequent_empty_lines" "2" >>=
    setValue "cpp" "clang_options" (intercalate ",\n  " $ clangOptions root useSystemHeaders compiler)
  where
#ifdef CLANG_INLINER
    defaultLanguage = "cpp"
#else
    defaultLanguage = "simplecpp"
#endif

-- Bundled headers
clangOptions :: FilePath -> Bool -> SystemCompilerInfo -> [String]
clangOptions root False _ =
    [ "-target"
    , "i386-pc-mingw32"
    , "-nostdinc"
    , "-isystem"
    , encodeString $ root </> "include" </> "c++"
    , "-isystem"
    , encodeString $ root </> "include" </> "c++" </> "i686-w64-mingw32"
    , "-isystem"
    , encodeString $ root </> "include" </> "crt"
    , "-I"
    , encodeString $ root </> "cpplib"
    , "-std=c++17"
    , "-fparse-all-comments"
    , "-DONLINE_JUDGE"
    ]

-- Windows with VS headers
clangOptions root True compiler | "mingw" `isPrefixOf` os =
    [ "-target"
    , "i386-pc-windows-msvc"
    , "-fdiagnostics-format=msvc"
    , "-fmsc-version=" ++ show (mscver compiler)
    , "-fparse-all-comments"
    , "-D_CRT_SECURE_NO_WARNINGS"
    , "-DONLINE_JUDGE"
    , "-I"
    , encodeString $ root </> "cpplib"
    ]

-- Linux with system headers
clangOptions root True compiler =
    [ "-target"
    , arch ++ "-" ++ os
    ] ++
    gccHeadersOptions (gccIncludeDirectories compiler) ++
    -- clang builtin headers are still required:
    -- https://clang.llvm.org/docs/LibTooling.html#libtooling-builtin-includes
    [ "-isystem"
    , encodeString $ root </> "include" </> "clang-builtins"
    , "-I"
    , encodeString $ root </> "cpplib"
    , "-fparse-all-comments"
    , "-DONLINE_JUDGE"
    ]

gccHeadersOptions :: [String] -> [String]
gccHeadersOptions [] = []
gccHeadersOptions includeDirectories = ["-nostdinc"] ++
    concat [["-isystem", dir] | dir <- includeDirectories]

forceEither :: Either a c -> c
forceEither = either (error "Left in forceEither") id

defaultCaideState :: ConfigParser
defaultCaideState = forceEither $
    addSection "core" emptyCP >>=
    setValue "core" "problem" ""

defaultProblemConfig :: ConfigParser
defaultProblemConfig = forceEither $
    addSection "problem" emptyCP >>=
    setValue "problem" "double_precision" "0.000001"

defaultProblemState :: ConfigParser
defaultProblemState = forceEither $
    addSection "problem" emptyCP >>=
    setValue "problem" "language" "c++"

