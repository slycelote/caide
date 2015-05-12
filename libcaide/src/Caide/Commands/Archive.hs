{-# LANGUAGE OverloadedStrings #-}
module Caide.Commands.Archive(
      archiveProblem
) where

import Prelude hiding (FilePath)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless, when, forM_, forM)
import Control.Monad.State (liftIO)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Time (getZonedTime, formatTime)
import Filesystem (isDirectory, createTree, removeTree, listDirectory, isFile)
import Filesystem.Path.CurrentOS ((</>), fromText, decodeString, basename, FilePath)
import System.IO.Error (catchIOError, ioeGetErrorString, isPermissionError)
import System.Locale (defaultTimeLocale)

import Caide.Commands.Checkout (checkoutProblem)
import Caide.Configuration (getActiveProblem, getFeatures)
import Caide.Registry (findFeature)
import Caide.Types
import Caide.Util (copyTreeToDir, copyFileToDir, listDir, pathToText, tshow, withLock)


archiveProblem :: ProblemID -> CaideIO ()
archiveProblem probId' = withLock $ do
    root <- caideRoot
    let probId = T.dropAround (\c -> c == '/' || c == '\\') probId'
        problemDir = root </> fromText probId
        problemStateDir = problemDir </> ".caideproblem"
    problemExists <- liftIO $ isFile $ problemDir </> "problem.ini"
    unless problemExists $ throw . T.concat $ ["Problem ", probId, " doesn't exist"]

    -- Prepare archive directory
    now <- liftIO getZonedTime
    let formattedDate = formatTime defaultTimeLocale "%F" now
        archiveDirToday = root </> "caide_archive" </> decodeString formattedDate

    liftIO $ do
        archiveDir <- appendNumberIfExists archiveDirToday probId
        createTree archiveDir
        -- Copy necessary files
        files <- fst <$> listDir problemDir
        forM_ files $ \file -> copyFileToDir file archiveDir
        copyTreeToDir problemStateDir archiveDir
        -- Remove problem directory
        -- A common error in Windows is when a problem folder is locked by
        -- Explorer or another program. In this case all files are removed but
        -- some of directories may be left. We ignore this exception (PermissionError).
        removeTree problemDir `catchIOError` (\e ->
            if isPermissionError e then putStrLn $ ioeGetErrorString e else ioError e)

    -- Change active problem, if necessary
    activeProblem <- getActiveProblem
    when (activeProblem == probId) $ do
        allProblems <- liftIO $ caideProblems root
        let newActiveProblem = if null allProblems then "" else head allProblems
        checkoutProblem newActiveProblem Nothing

    featureNames <- getFeatures
    let features = mapMaybe findFeature featureNames
    forM_ features (`onProblemRemoved` probId)


caideProblems :: FilePath -> IO [ProblemID]
caideProblems rootDir = do
    dirs <- listDirectory rootDir
    isCaideProblem <- forM dirs $ \dir -> isFile (dir </> "problem.ini")
    return $ sort [pathToText (basename dir) | (dir, True) <- zip dirs isCaideProblem]

appendNumberIfExists :: FilePath -> T.Text -> IO FilePath
appendNumberIfExists dir subdir = go ("": map (T.cons '~' . tshow) [1::Int ..])
  where
    go (prefix:rest) = do
        let fullPath = dir </> fromText (T.append subdir prefix)
        pathExists <- isDirectory fullPath
        if pathExists
        then go rest
        else return fullPath
    go [] = error "appendNumberIfExists: impossible happened"

