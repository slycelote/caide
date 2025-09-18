{-# LANGUAGE OverloadedStrings #-}
module Caide.Commands.Archive(
      archiveProblem
) where

import Prelude hiding (FilePath)
import Control.Monad.Extended (unless, when, forM_, forM, liftIO)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T

import Data.Time (defaultTimeLocale, getZonedTime, formatTime)

import Filesystem (isDirectory, createTree, removeTree, listDirectory, isFile)
import qualified Filesystem.Path.CurrentOS as FS
import Filesystem.Path.CurrentOS ((</>), FilePath)
import Filesystem.Util (copyTreeToDir, copyFileToDir, listDir, pathToText)

import System.IO.Error (catchIOError, ioeGetErrorString, isPermissionError)

import Caide.Commands.Checkout (checkoutProblem)
import Caide.GlobalState (readGlobalState, writeGlobalState, activeProblem)
import qualified Caide.GlobalTemplate as GlobalTemplate
import Caide.Monad (CaideIO, caideRoot, caideSettings, throw, Feature(onProblemRemoved))
import qualified Caide.Paths as Paths
import Caide.Registry (findFeature)
import Caide.Settings (enabledFeatureNames)
import Caide.Types
import Caide.Util (tshow, withLock)


archiveProblem :: ProblemID -> CaideIO ()
archiveProblem probId' = withLock $ do
    root <- caideRoot
    let probId = T.dropAround (\c -> c == '/' || c == '\\') probId'
        probDir = Paths.problemDir root probId
        problemStateDir = probDir </> ".caideproblem"
    problemExists <- liftIO $ isFile $ probDir </> "problem.ini"
    unless problemExists $ throw . T.concat $ ["Problem ", probId, " doesn't exist"]

    -- Prepare archive directory
    now <- liftIO getZonedTime
    let formattedDate = formatTime defaultTimeLocale "%F" now
        archiveDirToday = root </> "caide_archive" </> FS.decodeString formattedDate

    liftIO $ do
        archiveDir <- appendNumberIfExists archiveDirToday probId
        createTree archiveDir
        -- Copy necessary files
        files <- fst <$> listDir probDir
        forM_ files $ \file -> copyFileToDir file archiveDir
        copyTreeToDir problemStateDir archiveDir
        -- Remove problem directory
        -- A common error in Windows is when a problem folder is locked by
        -- Explorer or another program. In this case all files are removed but
        -- some of directories may be left. We ignore this exception (PermissionError).
        removeTree probDir `catchIOError` (\e ->
            if isPermissionError e then putStrLn $ ioeGetErrorString e else ioError e)

    -- Change active problem, if necessary
    globalState <- readGlobalState
    when (activeProblem globalState == Just probId) $ do
        allProblems <- liftIO $ caideProblems root
        case allProblems of
            (p:_) -> checkoutProblem p Nothing
            []    -> writeGlobalState $ globalState{activeProblem=Nothing}

    featureNames <- enabledFeatureNames <$> caideSettings
    let features = mapMaybe findFeature featureNames
    forM_ (GlobalTemplate.hook : features) (`onProblemRemoved` probId)


caideProblems :: FilePath -> IO [ProblemID]
caideProblems rootDir = do
    dirs <- listDirectory rootDir
    isCaideProblem <- forM dirs $ \dir -> isFile (dir </> "problem.ini")
    return $ sort [pathToText (FS.basename dir) | (dir, True) <- zip dirs isCaideProblem]

appendNumberIfExists :: FilePath -> T.Text -> IO FilePath
appendNumberIfExists dir subdir = go ("": map (T.cons '~' . tshow) [1::Int ..])
  where
    go (prefix:rest) = do
        let fullPath = dir </> FS.fromText (subdir <> prefix)
        pathExists <- isDirectory fullPath
        if pathExists
        then go rest
        else return fullPath
    go [] = error "appendNumberIfExists: impossible happened"

