module Caide.Commands.Archive (
      cmd
) where

import Prelude hiding (FilePath)
import Control.Applicative ((<$>))
import Control.Monad (unless, when, forM_, forM)
import Control.Monad.State (liftIO)
import Data.Time (getZonedTime, formatTime)
import Filesystem (isDirectory, createTree, removeTree, listDirectory, isDirectory)
import Filesystem.Path.CurrentOS ((</>), decodeString, encodeString, basename, FilePath)
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

    -- Prepare archive directory
    now <- liftIO getZonedTime
    let formattedDate = formatTime defaultTimeLocale "%F" now
        archiveDir = root </> decodeString "caide_archive" </> decodeString formattedDate </> decodeString probId
    archiveDirExists <- liftIO $ isDirectory archiveDir
    when archiveDirExists $ throw $ "Archive directory for this problem already exists: " ++ encodeString archiveDir

    liftIO $ do
        createTree archiveDir
        -- Copy necessary files
        files <- fst <$> listDir problemDir
        forM_ files $ \file -> copyFileToDir file archiveDir
        copyTreeToDir problemStateDir archiveDir
        -- Remove problem directory
        removeTree problemDir

    -- Change active problem, if necessary
    activeProblem <- getActiveProblem
    when (activeProblem == probId) $ do
        allProblems <- liftIO $ caideProblems root
        let newActiveProblem = if null allProblems then "" else head allProblems
        setActiveProblem newActiveProblem


archive _ = throw $ "Usage: " ++ usage cmd

caideProblems :: FilePath -> IO [ProblemID]
caideProblems rootDir = do
    dirs <- listDirectory rootDir
    isCaideProblem <- forM dirs $ \dir -> isDirectory (dir </> decodeString ".caideproblem")
    return [encodeString (basename dir) | (dir, True) <- zip dirs isCaideProblem]

