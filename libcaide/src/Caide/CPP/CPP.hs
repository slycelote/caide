{-# LANGUAGE OverloadedStrings #-}
module Caide.CPP.CPP(
      language
) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.State (liftIO)
import qualified Data.Text as T

import Prelude hiding (FilePath)
import Filesystem (copyFile, isDirectory)
import Filesystem.Path.CurrentOS (fromText)
import Filesystem.Path ((</>), FilePath, hasExtension)

import Text.Regex.TDFA.Text (Regex)
import Text.Regex.Base.RegexLike (makeRegex, match)

import Filesystem.Util (writeTextFile)

import qualified Caide.CPP.CPPSimple as CPPSimple

import Caide.Configuration (readCaideConf, withDefault)
import Caide.CPP.CBinding
import Caide.Types
import Caide.Util (listDir, readTextFile', tshow)


language :: ProgrammingLanguage
language = CPPSimple.language {inlineCode = inlineCPPCode}

inlineCPPCode :: ProblemID -> CaideIO ()
inlineCPPCode probID = do
    root <- caideRoot
    let problemDir = root </> fromText probID
        solutionPath = problemDir </> fromText (T.append probID ".cpp")
        mainFilePath = problemDir </> "main.cpp"
        concatCodePath = problemDir </> ".caideproblem" </> "concat.cpp"
        inlinedCodePath = problemDir </> ".caideproblem" </> "inlined.cpp"
        inlinedNoPragmaOnceCodePath = problemDir </> ".caideproblem" </> "inlinedNoPragmaOnce.cpp"
        finalCodePath = problemDir </> "submission.cpp"
        libraryDirectory = root </> "cpplib"

    hConf <- readCaideConf
    cmdLineOptions <- getProp hConf "cpp" "clang_options"
    macrosToKeep <- withDefault ["ONLINE_JUDGE"] $ getProp hConf "cpp" "keep_macros"

    libExists <- liftIO $ isDirectory libraryDirectory
    libraryCPPFiles <- if libExists
                       then filter (`hasExtension` "cpp") <$> liftIO (listDirectoryRecursively libraryDirectory)
                       else return []

    concatFiles <- T.concat <$> mapM readTextFile' (solutionPath:mainFilePath:libraryCPPFiles)
    liftIO $ writeTextFile concatCodePath concatFiles

    retInliner <- liftIO $ inlineLibraryCode [concatCodePath] cmdLineOptions inlinedCodePath
    when (retInliner /= 0) $
        throw $ T.concat ["C++ library code inliner failed with error code ", tshow retInliner]

    removePragmaOnceFromFile inlinedCodePath inlinedNoPragmaOnceCodePath

    retOptimizer <- liftIO $ removeUnusedCode inlinedNoPragmaOnceCodePath cmdLineOptions macrosToKeep finalCodePath
    when (retOptimizer /= 0) $
        throw $ T.concat ["C++ library code inliner (pass 1) failed with error code ", tshow retOptimizer]

    liftIO $ copyFile finalCodePath $ root </> "submission.cpp"


listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively dir = do
    (files, dirs) <- listDir dir
    recList <- concat <$> mapM listDirectoryRecursively dirs
    return $ files ++ recList

removePragmaOnceFromFile :: FilePath -> FilePath -> CaideIO ()
removePragmaOnceFromFile inputPath outputPath = do
    contents <- readTextFile' inputPath
    liftIO $ writeTextFile outputPath $ removePragmaOnce contents

pragmaOnceRegex :: Regex
pragmaOnceRegex = makeRegex ("^[[:space:]]*#[[:space:]]*pragma[[:space:]]+once[[:space:]]*$" :: T.Text)

removePragmaOnce :: T.Text -> T.Text
removePragmaOnce = T.unlines . filter (not . isPragmaOnce) . T.lines
    where isPragmaOnce = match pragmaOnceRegex

