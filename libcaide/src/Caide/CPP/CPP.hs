{-# LANGUAGE CPP, OverloadedStrings #-}
module Caide.CPP.CPP(
      language
) where

#ifndef AMP
import Control.Applicative ((<$>))
#endif
import Control.Monad.State (liftIO)
import Data.Char (isSpace)
import Data.Either (isRight)
import Data.List (groupBy)
import qualified Data.Text as T

import Prelude hiding (FilePath)
import Filesystem (copyFile, isDirectory)
import Filesystem.Path.CurrentOS (fromText)
import Filesystem.Path ((</>), FilePath, hasExtension)

import qualified Filesystem.Path.CurrentOS as F

import Text.Parsec
import Text.Parsec.Text (Parser)

import Filesystem.Util (listDir, writeTextFile)

import qualified Caide.CPP.CPPSimple as CPPSimple

import Caide.Configuration (readCaideConf, readProblemConfig, withDefault)
import Caide.CPP.CBinding
import Caide.Types
import Caide.Util (readTextFile', tshow)


language :: ProgrammingLanguage
language = CPPSimple.language {inlineCode = inlineCPPCode}

data InlinerStage = InlinerStage
    { stageName   :: T.Text
    , stageAction :: F.FilePath -> F.FilePath -> CaideIO Int
    }

doInline :: [InlinerStage] -> F.FilePath -> F.FilePath -> CaideIO F.FilePath
doInline [] _ f = return f
doInline (first:rest) tempDir f = do
    let nextFile = tempDir </> fromText (stageName first `T.append` ".cpp")
    ret <- stageAction first f nextFile
    case ret of
        0 -> doInline rest tempDir nextFile
        _ -> throw . T.concat $
            ["Inliner stage '", stageName first, "' failed with error code ", tshow ret]


inlineCPPCode :: ProblemID -> CaideIO ()
inlineCPPCode probID = do
    root <- caideRoot
    let problemDir = root </> fromText probID
        solutionPath = problemDir </> fromText (T.append probID ".cpp")
        mainFilePath = problemDir </> "main.cpp"
        tempDir = problemDir </> ".caideproblem"
        concatCodePath = problemDir </> ".caideproblem" </> "concat.cpp"
        libraryDirectory = root </> "cpplib"

    hConf <- readCaideConf
    cmdLineOptions <- getProp hConf "cpp" "clang_options"
    macrosToKeep <- withDefault ["ONLINE_JUDGE"] $ getProp hConf "cpp" "keep_macros"

    hProbConf <- readProblemConfig probID
    probType <- getProp hProbConf "problem" "type"

    libExists <- liftIO $ isDirectory libraryDirectory
    libraryCPPFiles <- if libExists
                       then filter (`hasExtension` "cpp") <$> liftIO (listDirectoryRecursively libraryDirectory)
                       else return []

    let allCppFiles = case probType of
            Stream _ _ -> solutionPath:mainFilePath:libraryCPPFiles
            Topcoder _ -> solutionPath:libraryCPPFiles

    concatFiles <- T.concat <$> mapM readTextFile' allCppFiles
    liftIO $ writeTextFile concatCodePath concatFiles

    let stages :: [InlinerStage]
        stages =
               [ InlinerStage
                   { stageName = "inline"
                   , stageAction = \input output ->
                        liftIO $ inlineLibraryCode [input] cmdLineOptions output
                   }
               , InlinerStage
                   { stageName = "removePragmaOnce"
                   , stageAction = \input output -> do
                        removePragmaOnceFromFile input output
                        return 0
                   }
               , InlinerStage
                   { stageName = "removeUnusedCode"
                   , stageAction = \input output ->
                        liftIO $ removeUnusedCode input cmdLineOptions macrosToKeep output
                   }
               , InlinerStage
                   { stageName = "removeEmptyLines"
                   , stageAction = \input output -> do
                        maxConsequentEmptyLines <- withDefault 2 $
                            getProp hConf "cpp" "max_consequent_empty_lines"
                        cont <- readTextFile' input
                        liftIO $ writeTextFile output $ removeEmptyLines maxConsequentEmptyLines cont
                        return 0
                   }
               ]

    finalCodePath <- doInline stages tempDir concatCodePath

    liftIO $ do
        copyFile finalCodePath $ problemDir </> "submission.cpp"
        copyFile finalCodePath $ root </> "submission.cpp"


listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively dir = do
    (files, dirs) <- listDir dir
    recList <- concat <$> mapM listDirectoryRecursively dirs
    return $ files ++ recList

removePragmaOnceFromFile :: FilePath -> FilePath -> CaideIO ()
removePragmaOnceFromFile inputPath outputPath = do
    contents <- readTextFile' inputPath
    liftIO $ writeTextFile outputPath $ removePragmaOnce contents

pragmaOnceLine :: Parser ()
pragmaOnceLine = do
    skipMany space
    _ <- char '#'
    skipMany space
    _ <- string "pragma"
    skipMany1 space
    _ <- string "once"
    skipMany space
    eof

removePragmaOnce :: T.Text -> T.Text
removePragmaOnce = T.unlines . filter (not . isPragmaOnce) . T.lines
    where isPragmaOnce = isRight . parse pragmaOnceLine "line_of_code"

removeEmptyLines :: Int -> T.Text -> T.Text
removeEmptyLines maxConsequentEmptyLines =
    -- concat
    T.unlines .
    -- leave no more than this number of empty lines
    concatMap (take maxConsequentEmptyLines) .
    -- group empty lines together
    groupBy bothEmptyLines .
    -- split
    T.lines
  where
    bothEmptyLines line1 line2 = all (T.all isSpace) [line1, line2]

