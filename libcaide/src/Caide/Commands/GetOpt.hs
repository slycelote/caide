module Caide.Commands.GetOpt(
      getOpt
    , getState
    , getProbOpt
    , getProbState
    , printRoot
) where

import Control.Monad.Extended (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO.Util as T

import Filesystem.Util (pathToText)

import Caide.Configuration (readConfigFile, getProp)
import Caide.Monad (CaideIO, caideRoot, rightOrThrow)
import Caide.Problem (readProblemCP, readProblemStateCP)
import qualified Caide.Paths as Paths

getOpt :: T.Text -> T.Text -> CaideIO ()
getOpt section key = do
    root <- caideRoot
    mbcp <- liftIO $ readConfigFile root Paths.caideConfFile
    cp <- rightOrThrow mbcp
    val <- rightOrThrow $ getProp cp (T.unpack section) (T.unpack key)
    liftIO $ T.putStrLn val

getState :: T.Text -> T.Text -> CaideIO ()
getState section key = do
    root <- caideRoot
    mbcp <- liftIO $ readConfigFile root Paths.caideStateFile
    cp <- rightOrThrow mbcp
    val <- rightOrThrow $ getProp cp (T.unpack section) (T.unpack key)
    liftIO $ T.putStrLn val


getProbOpt :: T.Text -> T.Text -> T.Text -> CaideIO ()
getProbOpt problem section key = do
    cp <- readProblemCP problem
    val <- rightOrThrow $ getProp cp (T.unpack section) (T.unpack key)
    liftIO $ T.putStrLn val

getProbState :: T.Text -> T.Text -> T.Text -> CaideIO ()
getProbState problem section key = do
    cp <- readProblemStateCP problem
    val <- rightOrThrow $ getProp cp (T.unpack section) (T.unpack key)
    liftIO $ T.putStrLn val


printRoot :: CaideIO ()
printRoot = caideRoot >>= liftIO . T.putStrLn . pathToText

