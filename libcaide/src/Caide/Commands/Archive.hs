{-# LANGUAGE OverloadedStrings #-}
module Caide.Commands.Archive (
      cmd
) where

import Prelude hiding (FilePath)
import Control.Applicative ((<$>))
import Control.Monad (unless, when, forM_, forM)
import Control.Monad.State (liftIO)
import Data.List (sort)
import qualified Data.Text as T
import Data.Time (getZonedTime, formatTime)
import Filesystem (isDirectory, createTree, removeTree, listDirectory, isDirectory)
import Filesystem.Path.CurrentOS ((</>), fromText, decodeString, basename, FilePath)
import System.Locale (defaultTimeLocale)

import qualified Caide.Commands.Checkout as Checkout
import Caide.Configuration (getActiveProblem)
import Caide.Types
import Caide.Util (copyTreeToDir, copyFileToDir, listDir, pathToText)

cmd :: CommandHandler
cmd = CommandHandler
    { command      = "archive"
    , description  = "Archive a problem"
    , usage        = "caide archive <problemID>"
    , action       = archive
    }

archive :: [T.Text] -> CaideIO ()
archive [probId] = do
    root <- caideRoot
    let problemDir = root </> fromText probId
        problemStateDir = problemDir </> ".caideproblem"
    problemExists <- liftIO $ isDirectory problemStateDir
    unless problemExists $ throw . T.concat $ ["Problem ", probId, " doesn't exist"]

    -- Prepare archive directory
    now <- liftIO getZonedTime
    let formattedDate = formatTime defaultTimeLocale "%F" now
        archiveDir = root </> "caide_archive" </> decodeString formattedDate </> fromText probId
    archiveDirExists <- liftIO $ isDirectory archiveDir
    when archiveDirExists $ throw . T.concat $
        ["Archive directory for this problem already exists: ", pathToText archiveDir]

    liftIO $ do
        createTree archiveDir
        -- Copy necessary files
        files <- fst <$> listDir problemDir
        forM_ files $ \file -> copyFileToDir file archiveDir
        copyTreeToDir problemStateDir archiveDir
        -- Remove problem directory
        removeTree problemDir

    -- TODO Notify features about archiving event

    -- Change active problem, if necessary
    activeProblem <- getActiveProblem
    when (activeProblem == probId) $ do
        allProblems <- liftIO $ caideProblems root
        let newActiveProblem = if null allProblems then "" else head allProblems
        action Checkout.cmd [newActiveProblem]


archive _ = throw . T.concat $ ["Usage: ", usage cmd]

caideProblems :: FilePath -> IO [ProblemID]
caideProblems rootDir = do
    dirs <- listDirectory rootDir
    isCaideProblem <- forM dirs $ \dir -> isDirectory (dir </> ".caideproblem")
    return $ sort [pathToText (basename dir) | (dir, True) <- zip dirs isCaideProblem]

