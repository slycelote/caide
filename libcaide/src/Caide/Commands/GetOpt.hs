module Caide.Commands.GetOpt(
      getOpt
    , getState
    , getProbOpt
    , getProbState
) where

import Control.Monad.State (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Caide.Configuration (readProblemConfig, readProblemState, readCaideConf, readCaideState)
import Caide.Types

getOpt :: T.Text -> T.Text -> CaideIO ()
getOpt section key = do
    h <- readCaideConf
    val <- getProp h (T.unpack section) (T.unpack key)
    liftIO $ T.putStrLn val

getState :: T.Text -> T.Text -> CaideIO ()
getState section key = do
    h <- readCaideState
    val <- getProp h (T.unpack section) (T.unpack key)
    liftIO $ T.putStrLn val


getProbOpt :: T.Text -> T.Text -> T.Text -> CaideIO ()
getProbOpt problem section key = do
    h <- readProblemConfig problem
    val <- getProp h (T.unpack section) (T.unpack key)
    liftIO $ T.putStrLn val

getProbState :: T.Text -> T.Text -> T.Text -> CaideIO ()
getProbState problem section key = do
    h <- readProblemState problem
    val <- getProp h (T.unpack section) (T.unpack key)
    liftIO $ T.putStrLn val

