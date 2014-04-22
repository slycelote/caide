module Caide.Configuration(
      Config
    , defaultConf
    , readRootConf
--    , readConfig
    , saveRootConf
--    , saveConfig
    , getOption
    , setOption
    , getActiveProblem
    , setActiveProblem
) where

import Control.Applicative ((<$>))
import Data.ConfigFile
import qualified Data.Text as T
import Filesystem (writeTextFile, createTree, isFile)
import qualified Filesystem.Path as F
import Filesystem.Path.CurrentOS (encodeString, decodeString, (</>))

import Caide.Types (ProblemID)
import Caide.Util (forceEither)

type Config = ConfigParser

readConfig :: F.FilePath -> IO Config
readConfig file = either (const defaultConf) id <$> readfile defaultConf (encodeString file)

readRootConf :: F.FilePath -> IO Config
readRootConf dir = do
    let confFile = dir </> decodeString ".caide" </> decodeString "config.ini"
    fileExists <- isFile confFile
    if fileExists
        then readConfig confFile
        else return defaultConf

saveConfig :: Config -> F.FilePath -> IO ()
saveConfig conf file = writeTextFile file $ T.pack $ to_string conf

saveRootConf :: F.FilePath -> Config -> IO ()
saveRootConf caideRoot conf = do
    let confDir = caideRoot </> decodeString ".caide"
    createTree confDir
    saveConfig conf $ confDir </> decodeString "config.ini"

getOptionOrDefault :: Get_C a => Config -> String -> String -> a -> a
getOptionOrDefault conf section key defaultValue = either (const defaultValue) id $ get conf section key

getOption :: Get_C a => Config -> String -> String -> a
getOption conf section key = getOptionOrDefault conf section key $ forceEither $ get defaultConf section key

setOption :: Config -> String -> String -> String -> Config
setOption conf section key val = forceEither $ set conf section key val

getActiveProblem :: Config -> String
getActiveProblem conf = getOption conf "core" "problem"

setActiveProblem :: Config -> ProblemID -> Config
setActiveProblem conf problemId = setOption conf "core" "problem" problemId

defaultConf :: Config
defaultConf = forceEither $ do
    let cp = emptyCP
    cp <- add_section cp "core"
    cp <- set cp "core" "language" "SimpleCPP"
    cp <- set cp "core" "problem" ""
    return cp
