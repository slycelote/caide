{-# LANGUAGE FlexibleContexts, Rank2Types #-}

module Caide.Configuration (
      -- * General utilities
      setProperties
    , getListProp
    , orDefault
    , describeError

      -- * Caide configuration
    , readCaideConf
    , readCaideState
    , writeCaideConf
    , writeCaideState
    , defaultCaideConf
    , defaultCaideState

      -- * Caide options and state
    , getActiveProblem
    , setActiveProblem
    , getDefaultLanguage
    , getBuilder
    , getFeatures

      -- * Problem configuration
    , getProblemConfigFile
    , readProblemConfig
    , getProblemStateFile
    , readProblemState
    , defaultProblemConfig
    , defaultProblemState
    , writeProblemState
    , writeProblemConf

) where

import Prelude hiding (readFile, FilePath)

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.Except (catchError)
import Control.Monad.Trans (liftIO)
import Data.ConfigFile (ConfigParser, CPError, CPErrorData(OtherProblem), SectionSpec, OptionSpec,
                        set, emptyCP, add_section)
import Data.List (intercalate, isPrefixOf)
import Filesystem (isDirectory)
import Filesystem.Path.CurrentOS (encodeString, decodeString)
import Filesystem.Path (FilePath, (</>))

import System.Info (arch, os)

import Caide.Types
import Caide.Util (forceEither, splitString, trimString)


setProperties :: Monad m => ConfigFileHandle -> [(String, String, String)] -> CaideM m ()
setProperties handle properties = forM_ properties $ \(section, key, value) ->
    setProp handle section key value

getListProp :: (Functor m, Monad m) => ConfigFileHandle -> String -> String -> CaideM m [String]
getListProp h section key = map trimString . splitString "," <$> getProp h section key

orDefault :: Monad m => CaideM m a -> a -> CaideM m a
orDefault getter defaultValue = getter `catchError` const (return defaultValue)

describeError :: CPError -> String
describeError (OtherProblem err, _) = err
describeError e                     = "Config parser error: " ++ show e

{--------------------------- Problem specific state ----------------------------}

getProblemStateFile :: Monad m => ProblemID -> CaideM m FilePath
getProblemStateFile probId = do
    root <- caideRoot
    return $ root </> decodeString probId </> decodeString ".caideproblem" </> decodeString "config"

readProblemState :: ProblemID -> CaideIO ConfigFileHandle
readProblemState probId = do
    root <- caideRoot
    problemExists <- liftIO $ isDirectory $ root </> decodeString probId </> decodeString ".caideproblem"
    if problemExists
    then getProblemStateFile probId >>= readConf
    else throw "No such problem"

getProblemConfigFile :: Monad m => ProblemID -> CaideM m FilePath
getProblemConfigFile probId = do
    root <- caideRoot
    return $ root </> decodeString probId </> decodeString "problem.ini"

readProblemConfig :: ProblemID -> CaideIO ConfigFileHandle
readProblemConfig probId = do
    root <- caideRoot
    problemExists <- liftIO $ isDirectory $ root </> decodeString probId </> decodeString ".caideproblem"
    if problemExists
    then getProblemConfigFile probId >>= readConf
    else throw "No such problem"

writeProblemConf :: Monad m => ProblemID -> CaideM m ConfigFileHandle
writeProblemConf probId = do
    filePath <- getProblemConfigFile probId
    createConf filePath defaultProblemConfig

writeProblemState :: Monad m => ProblemID -> CaideM m ConfigFileHandle
writeProblemState probId = do
    filePath <- getProblemStateFile probId
    createConf filePath defaultProblemState

{--------------------------- Global options and state ----------------------------}
caideConfFile :: Monad m => CaideM m FilePath
caideConfFile = do
    root <- caideRoot
    return $ root </> decodeString "caide.ini"

caideStateFile :: Monad m => CaideM m FilePath
caideStateFile = do
    root <- caideRoot
    return $ root </> decodeString ".caide" </> decodeString "config"

readCaideConf :: CaideIO ConfigFileHandle
readCaideConf = caideConfFile >>= readConf

readCaideState :: CaideIO ConfigFileHandle
readCaideState = caideStateFile >>= readConf

writeCaideConf :: Monad m => ConfigParser -> CaideM m ConfigFileHandle
writeCaideConf cp = do
    filePath <- caideConfFile
    createConf filePath cp

writeCaideState :: Monad m => ConfigParser -> CaideM m ConfigFileHandle
writeCaideState cp = do
    filePath <- caideStateFile
    createConf filePath cp

getActiveProblem :: CaideIO ProblemID
getActiveProblem = do
    h <- readCaideState
    res <- getProp h "core" "problem" `orDefault` ""
    if null res
    then throw "No active problem. Switch to an existing problem with `caide checkout <problemID>`"
    else return res

setActiveProblem :: ProblemID -> CaideIO ()
setActiveProblem probId = do
    h <- readCaideState
    setProp h "core" "problem" probId

getBuilder :: CaideIO String
getBuilder = do
    h <- readCaideConf
    getProp h "core" "builder"

getDefaultLanguage :: CaideIO String
getDefaultLanguage = do
    h <- readCaideConf
    getProp h "core" "language"

getFeatures :: CaideIO [String]
getFeatures = do
    h <- readCaideConf
    getListProp h "core" "features"

{--------------------------- Internals -----------------------------}

addSection :: SectionSpec -> ConfigParser -> Either CPError ConfigParser
addSection section conf = add_section conf section

setValue :: SectionSpec -> OptionSpec -> String -> ConfigParser -> Either CPError ConfigParser
setValue section key value conf = set conf section key value

defaultCaideConf :: FilePath -> Bool -> ConfigParser
defaultCaideConf root useSystemHeaders = forceEither $
    addSection "core" emptyCP >>=
    setValue "core" "language" "cpp" >>=
    setValue "core" "features" "" >>=
    setValue "core" "builder" "none" >>=
    addSection "cpp" >>=
    setValue "cpp" "clang_options" (intercalate ",\n  " $ clangOptions root useSystemHeaders)

clangOptions :: FilePath -> Bool -> [String]
clangOptions root False = [
    "-target",
    "i386-pc-mingw32",
    "-nostdinc",
    "-isystem",
    encodeString $ root </> decodeString "include" </> decodeString "mingw-4.8.1",
    "-isystem",
    encodeString $ root </> decodeString "include" </> decodeString "mingw-4.8.1" </> decodeString "c++",
    "-isystem",
    encodeString $ root </> decodeString "include" </> decodeString "mingw-4.8.1" </> decodeString "c++" </> decodeString "mingw32",
    "-isystem",
    encodeString $ root </> decodeString "include",
    "-I",
    encodeString $ root </> decodeString "cpplib",
    "-std=c++11",
    "-D__MSVCRT__=1",
    "_D__declspec="
    ]

clangOptions root True | "mingw" `isPrefixOf` os = [
    "-target",
    "i386-pc-windows-msvc",
    "-I",
    encodeString $ root </> decodeString "cpplib"
    ]

clangOptions root True = [
    "-target",
    arch ++ "-" ++ os,
    "-I",
    encodeString $ root </> decodeString "cpplib"
    ]


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
    setValue "problem" "language" "simplecpp"

