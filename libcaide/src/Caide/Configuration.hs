module Caide.Configuration(
      Config
    , getOption
    , getOptionOrDefault
    , setOption

    -- * Caide configuration
    , readRootConf
    , saveRootConf
    , getActiveProblem
    , setActiveProblem
    , getDefaultLanguage
    , getBuilder

    -- * Problem Configuration
    , readProblemConf
    , saveProblemConf
) where

import Prelude hiding (readFile)

import Control.Applicative ((<$>))
import Data.ConfigFile
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Filesystem (writeTextFile, createTree, isFile)
import qualified Filesystem.Path as F
import Filesystem.Path.CurrentOS (encodeString, decodeString, (</>))


import Caide.Types (ProblemID)
import Caide.Util (forceEither)

type Config = ConfigParser

readConfig :: F.FilePath -> Config -> IO Config
readConfig file def = either (const def) id . readstring def . T.unpack <$>
    readFile (encodeString file)

readRootConf :: F.FilePath -> IO Config
readRootConf dir = do
    let confFile = dir </> decodeString ".caide" </> decodeString "config.ini"
    fileExists <- isFile confFile
    if fileExists
        then readConfig confFile defaultConf
        else return defaultConf

readProblemConf :: F.FilePath -> IO Config
readProblemConf dir = do
    let confFile = dir </> decodeString ".caideproblem" </> decodeString "config.ini"
    fileExists <- isFile confFile
    if fileExists
        then readConfig confFile defaultProblemConf
        else return defaultProblemConf

saveConfig :: Config -> F.FilePath -> IO ()
saveConfig conf file = writeTextFile file $ T.pack $ to_string conf

saveRootConf :: F.FilePath -> Config -> IO ()
saveRootConf caideRoot conf = do
    let confDir = caideRoot </> decodeString ".caide"
    createTree confDir
    saveConfig conf $ confDir </> decodeString "config.ini"

saveProblemConf :: F.FilePath -> Config -> IO ()
saveProblemConf caideRoot conf = do
    let confDir = caideRoot </> decodeString ".caideproblem"
    createTree confDir
    saveConfig conf $ confDir </> decodeString "config.ini"

getOptionOrDefault :: Get_C a => Config -> String -> String -> a -> a
getOptionOrDefault conf section key defaultValue = either (const defaultValue) id $ get conf section key

getOption :: Get_C a => Config -> String -> String -> a
getOption conf section key = forceEither $ get conf section key

setOption :: Config -> String -> String -> String -> Config
setOption conf section key val = forceEither $ set conf section key val

getActiveProblem :: Config -> String
getActiveProblem conf = getOption conf "core" "problem"

setActiveProblem :: Config -> ProblemID -> Config
setActiveProblem conf = setOption conf "core" "problem"

getBuilder :: Config -> String
getBuilder conf = getOption conf "core" "builder"

getDefaultLanguage :: Config -> String
getDefaultLanguage conf = getOption conf "core" "language"

defaultConf :: Config
defaultConf = forceEither $ do
   let cp = emptyCP
   cp <- add_section cp "core"
   cp <- set cp "core" "language" "simplecpp"
   cp <- set cp "core" "builder" "none"
   set cp "core" "problem" ""

defaultProblemConf :: Config
defaultProblemConf = forceEither $ do
    let cp = emptyCP
    cp <- add_section cp "problem"
    set cp "problem" "language" "simplecpp"
