{-# LANGUAGE FlexibleContexts #-}
module Caide.Configuration (
      readCaideProject

    -- * Caide configuration
    , getActiveProblem
    , setActiveProblem
    , getDefaultLanguage
    , getBuilder
    , getFeatures

    -- * Problem Configuration
    , ProblemConfig
    , getProblemConfigFile
    , getProblemConfigFileInDir
    , readProblemConfig
    , saveProblemConfig
    , getProblemOption
    , setProblemOption
) where

import Prelude hiding (readFile, FilePath)

import Control.Applicative ((<$>))
import Control.Monad.Error (MonadError)
import Data.ConfigFile
import Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Filesystem (writeTextFile, createTree, isFile)
import Filesystem.Path.CurrentOS (encodeString, decodeString)
import Filesystem.Path (FilePath, (</>), directory)

import System.Environment (getExecutablePath)


import Caide.Types (ProblemID, CaideProject (..), getInternalOption, setInternalOption, getUserOption)
import Caide.Util (forceEither, splitString)


{--------------------------- Problem specific state ----------------------------}

type ProblemConfig = ConfigParser

getProblemConfigFile :: CaideProject -> ProblemID -> FilePath
getProblemConfigFile project probId = getProblemConfigFileInDir $ getRootDirectory project </> decodeString probId

getProblemConfigFileInDir :: FilePath -> FilePath
getProblemConfigFileInDir problemDir = problemDir </> decodeString ".caideproblem" </> decodeString "config"

readProblemConfig :: FilePath -> IO ProblemConfig
readProblemConfig file = readConfigWithDefault file defaultProblemConf

saveProblemConfig :: ProblemConfig -> FilePath -> IO ()
saveProblemConfig = saveConfig

getProblemOption :: ProblemConfig -> String -> String -> String
getProblemOption = getOption

setProblemOption :: ProblemConfig -> String -> String -> String -> ProblemConfig
setProblemOption = setOption


{--------------------------- Global options and state ----------------------------}

readCaideProject :: FilePath -> IO CaideProject
readCaideProject caideRoot = do
    let rootConfigFile = caideRoot </> decodeString "caide.ini"
        internalConfigFile = caideRoot </> decodeString ".caide" </> decodeString "config"

        readConf :: FilePath -> ConfigParser -> IO (IORef ConfigParser)
        readConf confFile defaultConf = readConfigWithDefault confFile defaultConf >>= newIORef

        saveConf :: IORef ConfigParser -> FilePath -> IO ()
        saveConf conf file = do
            c <- readIORef conf
            saveConfig c file

        getOpt c s k = readIORef c >>= \conf -> return $ getOption conf s k
        setOpt c s k v = modifyIORef c $ \conf -> setOption conf s k v


    caideExe <- getExecutablePath
    rootConf <- readConf rootConfigFile (defaultRootConf caideRoot)
    internalConf <- readConf internalConfigFile (defaultInternalConf caideExe)
    return CaideProject
        { getUserOption     = getOpt rootConf
        , getInternalOption = getOpt internalConf
        , setInternalOption = setOpt internalConf
        , getRootDirectory  = caideRoot
        , saveProject       = saveConf internalConf internalConfigFile
        }


getActiveProblem :: CaideProject -> IO String
getActiveProblem conf = getInternalOption conf "core" "problem"

setActiveProblem :: CaideProject -> String -> IO ()
setActiveProblem conf = setInternalOption conf "core" "problem"

getBuilder :: CaideProject -> IO String
getBuilder conf = getUserOption conf "core" "builder"

getDefaultLanguage :: CaideProject -> IO String
getDefaultLanguage conf = getUserOption conf "core" "language"

getFeatures :: CaideProject -> IO [String]
getFeatures conf = splitString ", " <$> getUserOption conf "core" "features"

{--------------------------- Internals -----------------------------}

readConfigWithDefault :: FilePath -> ConfigParser -> IO ConfigParser
readConfigWithDefault file def = do
    fileExists <- isFile file
    if fileExists
        then either (const def) id . readstring def . T.unpack <$> readFile (encodeString file)
        else do
            saveConfig def file
            return def


saveConfig :: ConfigParser -> FilePath -> IO ()
saveConfig conf file = do
    createTree $ directory file
    writeTextFile file $ T.pack $ to_string conf

getOption :: Get_C a => ConfigParser -> String -> String -> a
getOption conf section key = case get conf section key of
    Left err  -> error $ show err
    Right val -> val

setOption :: ConfigParser -> String -> String -> String -> ConfigParser
setOption conf section key val = forceEither $ set conf section key val

addSection :: MonadError CPError m => SectionSpec -> ConfigParser -> m ConfigParser
addSection section conf = add_section conf section

setValue :: MonadError CPError m => SectionSpec -> OptionSpec -> String -> ConfigParser -> m ConfigParser
setValue section key value conf = set conf section key value

defaultRootConf :: FilePath -> ConfigParser
defaultRootConf caideRoot = forceEither $
    addSection "core" emptyCP >>=
    setValue "core" "language" "cpp" >>=
    setValue "core" "features" "" >>=
    setValue "core" "builder" "none" >>=
    addSection "cpp" >>=
    setValue "cpp" "system_header_dirs" (intercalate "," $ map encodeString headerDirs)

    where
    headerDirs = [
        caideRoot </> decodeString "include" </> decodeString "mingw-4.8.1",
        caideRoot </> decodeString "include" </> decodeString "mingw-4.8.1" </> decodeString "c++",
        caideRoot </> decodeString "include" </> decodeString "mingw-4.8.1" </> decodeString "c++" </> decodeString "mingw32",
        caideRoot </> decodeString "include"]


defaultInternalConf :: String -> ConfigParser
defaultInternalConf caideExe = forceEither $
    addSection "core" emptyCP >>=
    setValue "core" "problem" "" >>=
    setValue "core" "caide_exe" caideExe

defaultProblemConf :: ConfigParser
defaultProblemConf = forceEither $
    addSection "problem" emptyCP >>=
    setValue "problem" "language" "simplecpp"

