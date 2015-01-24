{-# LANGUAGE OverloadedStrings #-}
module Caide.CPP.CPP (
      language
) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.State (liftIO)
import qualified Data.Text as T

import Prelude hiding (FilePath)
import Filesystem (copyFile, readTextFile, writeTextFile, isDirectory)
import Filesystem.Path.CurrentOS (fromText)
import Filesystem.Path ((</>), FilePath, hasExtension)

import Text.Regex.TDFA.Text (Regex)
import Text.Regex.Base.RegexLike (makeRegex, match)


import qualified Caide.CPP.CPPSimple as CPPSimple

import Caide.Configuration (readCaideConf)
import Caide.CPP.CBinding
import Caide.Types
import Caide.Util (listDir, tshow)


language :: ProgrammingLanguage
language = CPPSimple.language {inlineCode = inlineCPPCode}

inlineCPPCode :: ProblemID -> CaideIO ()
inlineCPPCode probID = do
    root <- caideRoot
    let problemDir = root </> fromText probID
        solutionPath = problemDir </> fromText (T.append probID ".cpp")
        mainFilePath = problemDir </> "main.cpp"
        inlinedCodePath = problemDir </> ".caideproblem" </> "inlined.cpp"
        inlinedNoPragmaOnceCodePath = problemDir </> ".caideproblem" </> "inlinedNoPragmaOnce.cpp"
        finalCodePath = problemDir </> "submission.cpp"
        libraryDirectory = root </> "cpplib"

    hConf <- readCaideConf
    cmdLineOptions <- getProp hConf "cpp" "clang_options"

    libExists <- liftIO $ isDirectory libraryDirectory
    libraryCPPFiles <- if libExists
                       then filter (`hasExtension` "cpp") <$> liftIO (listDirectoryRecursively libraryDirectory)
                       else return []

    retInliner <- liftIO $ inlineLibraryCode (solutionPath:mainFilePath:libraryCPPFiles) cmdLineOptions inlinedCodePath
    when (retInliner /= 0) $
        throw $ T.concat ["C++ library code inliner failed with error code ", tshow retInliner]
    liftIO $ removePragmaOnceFromFile inlinedCodePath inlinedNoPragmaOnceCodePath
    retOptimizer <- liftIO $ removeUnusedCode inlinedNoPragmaOnceCodePath cmdLineOptions finalCodePath
    when (retOptimizer /= 0) $
        throw $ T.concat ["C++ library code inliner failed with error code ", tshow retOptimizer]
    liftIO $ copyFile finalCodePath $ root </> "submission.cpp"


listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively dir = do
    (files, dirs) <- listDir dir
    recList <- concat <$> mapM listDirectoryRecursively dirs
    return $ files ++ recList

removePragmaOnceFromFile :: FilePath -> FilePath -> IO ()
removePragmaOnceFromFile inputPath outputPath =
    readTextFile inputPath >>= writeTextFile outputPath . removePragmaOnce

pragmaOnceRegex :: Regex
pragmaOnceRegex = makeRegex ("^[[:space:]]*#[[:space:]]*pragma[[:space:]]+once[[:space:]]*$" :: T.Text)

removePragmaOnce :: T.Text -> T.Text
removePragmaOnce = T.unlines . filter (not . isPragmaOnce) . T.lines
    where isPragmaOnce = match pragmaOnceRegex

