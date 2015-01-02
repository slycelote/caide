module Caide.Commands.Archive (
      cmd
) where

import Control.Applicative ((<$>))
import Control.Monad (unless, when, forM_)
import Control.Monad.State (liftIO)
import Data.Time (getZonedTime, formatTime)
import Filesystem (isDirectory, createTree, removeTree)
import Filesystem.Path.CurrentOS ((</>), decodeString, encodeString)
import System.Locale (defaultTimeLocale)

import Caide.Configuration (getActiveProblem, setActiveProblem)
import Caide.Types
import Caide.Util (copyTreeToDir, copyFileToDir, listDir)

cmd :: CommandHandler
cmd = CommandHandler
    { command      = "archive"
    , description  = "Archive a problem"
    , usage        = "caide archive <problemID>"
    , action       = archive
    }

archive :: [String] -> CaideIO ()
archive [probId] = do
    root <- caideRoot
    let problemDir = root </> decodeString probId
        problemStateDir = problemDir </> decodeString ".caideproblem"
    problemExists <- liftIO $ isDirectory problemStateDir
    unless problemExists $ throw $ "Problem " ++ probId ++ " doesn't exist"
    now <- liftIO getZonedTime
    let formattedDate = formatTime defaultTimeLocale "%F" now
        archiveDir = root </> decodeString "caide_archive" </> decodeString formattedDate </> decodeString probId
    archiveDirExists <- liftIO $ isDirectory archiveDir
    when archiveDirExists $ throw $ "Archive directory for this problem already exists: " ++ encodeString archiveDir
    liftIO $ do
        createTree archiveDir
        files <- fst <$> listDir problemDir
        forM_ files $ \file -> copyFileToDir file archiveDir
        copyTreeToDir problemStateDir archiveDir
        removeTree problemDir
    activeProblem <- getActiveProblem
    when (activeProblem == probId) $ setActiveProblem ""


archive _ = throw $ "Usage: " ++ usage cmd

