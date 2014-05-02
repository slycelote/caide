module Caide.Configuration (
      readCaideProject

    -- * Caide configuration
    , getActiveProblem
    , setActiveProblem
    , getDefaultLanguage
    , getBuilder

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
import Data.ConfigFile
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Filesystem (writeTextFile, createTree, isFile)
import Filesystem.Path.CurrentOS (encodeString, decodeString)
import Filesystem.Path (FilePath, (</>), directory)


import Caide.Types (ProblemID, CaideProject (..), getInternalOption, setInternalOption, getUserOption)
import Caide.Util (forceEither)
import Data.IORef (IORef, newIORef, modifyIORef, readIORef)


{------------------------ Problem specific state -----------------------}

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


{--------------------------- Global options and state -------------------------------}

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


    rootConf <- readConf rootConfigFile defaultRootConf
    internalConf <- readConf internalConfigFile defaultInternalConf
    return CaideProject
        { getUserOption     = getOpt rootConf
        , getInternalOption = getOpt internalConf
        , setInternalOption = setOpt internalConf
        , getRootDirectory  = caideRoot
        , saveProject       = saveConf rootConf rootConfigFile >> saveConf internalConf internalConfigFile  
        }


getActiveProblem :: CaideProject -> IO String
getActiveProblem conf = getInternalOption conf "core" "problem"

setActiveProblem :: CaideProject -> String -> IO ()
setActiveProblem conf = setInternalOption conf "core" "problem"

getBuilder :: CaideProject -> IO String
getBuilder conf = getUserOption conf "core" "builder"

getDefaultLanguage :: CaideProject -> IO String
getDefaultLanguage conf = getUserOption conf "core" "language"



{--------------------------- Internals -------------------------------}

readConfigWithDefault :: FilePath -> ConfigParser -> IO ConfigParser
readConfigWithDefault file def = do
    fileExists <- isFile file
    if fileExists
        then either (const def) id . readstring def . T.unpack <$> readFile (encodeString file)
        else return def


saveConfig :: ConfigParser -> FilePath -> IO ()
saveConfig conf file = do
    createTree $ directory file
    writeTextFile file $ T.pack $ to_string conf

getOption :: Get_C a => ConfigParser -> String -> String -> a
getOption conf section key = forceEither $ get conf section key

setOption :: ConfigParser -> String -> String -> String -> ConfigParser
setOption conf section key val = forceEither $ set conf section key val


defaultRootConf :: ConfigParser
defaultRootConf = forceEither $ do
   let cp = emptyCP
   cp <- add_section cp "core"
   cp <- set cp "core" "language" "simplecpp"
   set cp "core" "builder" "none"

defaultInternalConf :: ConfigParser
defaultInternalConf = forceEither $ do
    let cp = emptyCP
    cp <- add_section cp "core"
    set cp "core" "problem" ""

defaultProblemConf :: ConfigParser
defaultProblemConf = forceEither $ do
    let cp = emptyCP
    cp <- add_section cp "problem"
    set cp "problem" "language" "simplecpp"
