{-# LANGUAGE CPP, OverloadedStrings #-}

module Caide.Configuration(
      -- * General utilities
      setProperties
    , orDefault
    , withDefault
    , describeError

      -- * Caide configuration
    , readCaideConf
    , readCaideState
    , writeCaideConf
    , writeCaideState
    , SystemCompilerInfo(..)
    , defaultCaideConf
    , defaultCaideState

      -- * Caide options and state
    , getActiveProblem
    , setActiveProblem
    , getDefaultLanguage
    , getFeatures

      -- * Problem configuration
    , getProblemConfigFile
    , readProblemConfig
    , getProblemStateFile
    , readProblemState
    , defaultProblemConfig
    , defaultProblemState

) where

import Prelude hiding (readFile, FilePath)

import Control.Monad (forM_)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Trans (liftIO)
import Data.ConfigFile (ConfigParser, CPError, CPErrorData(OtherProblem, NoOption, NoSection),
                        SectionSpec, OptionSpec, set, emptyCP, add_section)
import Data.List (intercalate, isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Filesystem (isDirectory)
import Filesystem.Path.CurrentOS (encodeString, fromText)
import Filesystem.Path (FilePath, (</>))

import System.Info (arch, os)

import Caide.Types


setProperties :: Monad m => ConfigFileHandle c -> [(String, String, Text)] -> CaideM m ()
setProperties handle properties = forM_ properties $ \(section, key, value) ->
    setProp handle section key value

rethrowIfFatal :: Monad m => CPError -> CaideM m ()
rethrowIfFatal (NoSection _, _) = return ()
rethrowIfFatal (NoOption _, _) = return ()
rethrowIfFatal e = throwError e

orDefault :: Monad m => CaideM m a -> a -> CaideM m a
orDefault getter defaultValue = getter `catchError` \e -> do
    rethrowIfFatal e
    return defaultValue

withDefault :: Monad m => a -> CaideM m a -> CaideM m a
withDefault = flip orDefault

describeError :: CPError -> String
describeError (OtherProblem err, _) = err
describeError e                     = "Config parser error: " ++ show e

{--------------------------- Problem specific state ----------------------------}

getProblemStateFile :: Monad m => ProblemID -> CaideM m FilePath
getProblemStateFile probId = do
    root <- caideRoot
    return $ root </> fromText probId </> ".caideproblem" </> "config"

readProblemState :: ProblemID -> CaideIO (ConfigFileHandle Persistent)
readProblemState probId = do
    root <- caideRoot
    problemExists <- liftIO $ isDirectory $ root </> fromText probId </> ".caideproblem"
    if problemExists
    then getProblemStateFile probId >>= readConf
    else throw "No such problem"

getProblemConfigFile :: Monad m => ProblemID -> CaideM m FilePath
getProblemConfigFile probId = do
    root <- caideRoot
    return $ root </> fromText probId </> "problem.ini"

readProblemConfig :: ProblemID -> CaideIO (ConfigFileHandle Persistent)
readProblemConfig probId = do
    root <- caideRoot
    problemExists <- liftIO $ isDirectory $ root </> fromText probId </> ".caideproblem"
    if problemExists
    then getProblemConfigFile probId >>= readConf
    else throw "No such problem"


{--------------------------- Global options and state ----------------------------}
caideConfFile :: Monad m => CaideM m FilePath
caideConfFile = do
    root <- caideRoot
    return $ root </> "caide.ini"

caideStateFile :: Monad m => CaideM m FilePath
caideStateFile = do
    root <- caideRoot
    return $ root </> ".caide" </> "config"

readCaideConf :: CaideIO (ConfigFileHandle Persistent)
readCaideConf = caideConfFile >>= readConf

readCaideState :: CaideIO (ConfigFileHandle Persistent)
readCaideState = caideStateFile >>= readConf

writeCaideConf :: Monad m => ConfigParser -> CaideM m (ConfigFileHandle Persistent)
writeCaideConf cp = do
    filePath <- caideConfFile
    createConf filePath cp

writeCaideState :: Monad m => ConfigParser -> CaideM m (ConfigFileHandle Persistent)
writeCaideState cp = do
    filePath <- caideStateFile
    createConf filePath cp

getActiveProblem :: CaideIO ProblemID
getActiveProblem = do
    h <- readCaideState
    res <- getProp h "core" "problem" `orDefault` ""
    if T.null res
    then throw "No active problem. Switch to an existing problem with `caide checkout <problemID>`"
    else return res

setActiveProblem :: ProblemID -> CaideIO ()
setActiveProblem probId = do
    h <- readCaideState
    setProp h "core" "problem" probId

getDefaultLanguage :: CaideIO Text
getDefaultLanguage = do
    h <- readCaideConf
    getProp h "core" "language"

getFeatures :: CaideIO [Text]
getFeatures = do
    h <- readCaideConf
    getProp h "core" "features"

{--------------------------- Internals -----------------------------}

addSection :: SectionSpec -> ConfigParser -> Either CPError ConfigParser
addSection section conf = add_section conf section

setValue :: SectionSpec -> OptionSpec -> String -> ConfigParser -> Either CPError ConfigParser
setValue section key value conf = set conf section key value

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

