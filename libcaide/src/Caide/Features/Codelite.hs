module Caide.Features.Codelite (
      feature
) where

import Prelude hiding (readFile)

import Control.Monad (forM_, when, unless)
import Control.Monad.State.Strict (execState, modify, State)
import Control.Monad.State (liftIO)
import qualified Data.Text as T
import Data.Text.IO (readFile)

import Filesystem (isFile, writeTextFile, listDirectory, createDirectory)
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import Filesystem.Path ((</>), basename)

import Text.XML.Light (parseXML, Content(..))
import Text.XML.Light.Cursor

import Caide.Types
import Caide.Xml (goToChild, removeChildren, isTag, insertLastChild, mkElem, modifyFromJust,
                  changeAttr, hasAttrEqualTo, goToDocRoot, showXml)
import Caide.Configuration (readProblemConfig, getActiveProblem)

feature :: Feature
feature = Feature
    { onProblemCreated = \_ -> return ()
    , onProblemCodeCreated = generateProject
    , onProblemCheckedOut = const generateWorkspace
    }

generateProject :: ProblemID -> CaideIO ()
generateProject probId = do
    hProblem <- readProblemConfig probId
    lang <- getProp hProblem "problem" "language"
    when (lang `elem` ["simplecpp", "cpp", "c++"]) $ do
        liftIO $ putStrLn "Generating codelite project"
        croot <- caideRoot

        let projectFile = croot </> decodeString probId </> decodeString (probId ++ ".project")
            needLibrary = lang `elem` ["cpp", "c++"]
            libProjectDir  = croot </> decodeString "cpplib"
            libProjectFile = libProjectDir </> decodeString "cpplib.project"

        when needLibrary $ liftIO $ do
            libProjectExists <- isFile libProjectFile
            unless libProjectExists $ do
                createDirectory False libProjectDir
                xmlString <- readFile . encodeString $ croot </> decodeString "templates" </> decodeString "codelite_project_template.project"
                let doc = parseXML xmlString
                    Just cursor = fromForest doc
                    files = []
                    includePaths = ["."]
                    libs = []
                    libraryPaths = []
                    transformed = execState (generateProjectXML "cpplib" files includePaths libraryPaths libs) cursor
                transformed `seq` writeTextFile libProjectFile . T.pack . showXml $ transformed
                putStrLn "cpplib.project for Codelite successfully generated."

        projectExists <- liftIO $ isFile projectFile
        if projectExists
        then liftIO $ putStrLn $ probId ++ ".project already exists. Not overwriting."
        else do
            liftIO $ do
                xmlString <- readFile . encodeString $ croot </> decodeString "templates" </> decodeString "codelite_project_template.project"
                let doc = parseXML xmlString
                    Just cursor = fromForest doc
                    files = [probId ++ ".cpp", probId ++ "_test.cpp"]
                    includePaths = "." : ["../cpplib" | needLibrary]
                    libs = ["cpplib" | needLibrary]
                    libraryPaths = ["../cpplib/$(ConfigurationName)" | needLibrary]
                    transformed = execState (generateProjectXML probId files includePaths libraryPaths libs) cursor
                transformed `seq` writeTextFile projectFile . T.pack . showXml $ transformed
                putStrLn $ probId ++ ".project for Codelite successfully generated."
            generateWorkspace

generateProjectXML :: String -> [String] -> [String] -> [String] -> [String] -> State Cursor ()
generateProjectXML projectName sourceFiles includePaths libPaths libs = do
    modifyFromJust $ findRight (isTag "Codelite_Project")
    changeAttr "Name" projectName
    modifyFromJust $ findChild $ \c -> isTag "VirtualDirectory" c && hasAttrEqualTo "Name" "src" c
    removeChildren $ isTag "File"
    forM_ sourceFiles $ \file -> do
         errorIfFailed "Couldn't insert File element" $
            insertLastChild $ Elem $ mkElem "File" [("Name", file)]
         modifyFromJust parent

    modifyFromJust parent
    errorIfFailed "Couldn't find GlobalSettings/Compiler node" $
        goToChild ["Settings", "GlobalSettings", "Compiler"]

    forM_ includePaths $ \path -> do
        errorIfFailed "Coudln't insert include path" $
            insertLastChild $ Elem $ mkElem "IncludePath" [("Value", path)]
        modifyFromJust parent -- <Compiler>
    modifyFromJust parent -- <GlobalSettings>

    errorIfFailed "Couldn't find GlobalSettings/Linker node" $
        goToChild ["Linker"]
    forM_ libPaths $ \libPath -> do
        errorIfFailed "Couldn't insert library path" $
            insertLastChild $ Elem $ mkElem "LibraryPath" [("Value", libPath)]
        modifyFromJust parent -- <Linker>
    forM_ libs $ \lib -> do
        errorIfFailed "Couldn't insert library" $
            insertLastChild $ Elem $ mkElem "Library" [("Value", lib)]
        modifyFromJust parent -- <Linker>


    goToDocRoot

generateWorkspace :: CaideIO ()
generateWorkspace = do
    croot <- caideRoot
    projects <- getCodeliteProjects
    activeProblem <- getActiveProblem
    let workspaceFile = croot </> decodeString "caide.workspace"

    liftIO $ do
        workspaceExists <- isFile workspaceFile
        let existingWorkspace = if workspaceExists
            then workspaceFile
            else croot </> decodeString "templates" </> decodeString "codelite_workspace_template.workspace"
        xmlString <- readFile $ encodeString existingWorkspace
        let doc = parseXML xmlString
            Just cursor = fromForest doc
            transformed = execState (generateWorkspaceXml projects activeProblem) cursor
        transformed `seq` writeTextFile workspaceFile . T.pack . showXml $ transformed

-- Includes problems and CPP library
getCodeliteProjects :: CaideIO [String]
getCodeliteProjects = do
    croot <- caideRoot
    liftIO $ do
        dirs <- listDirectory croot
        let problemIds = map (encodeString . basename) dirs
            haveCodelite probId = isFile $ croot </> decodeString probId </> decodeString (probId ++ ".project")
        projectExists <- mapM haveCodelite problemIds
        return [probId | (probId, True) <- zip problemIds projectExists]


errorIfFailed :: Monad m => String -> m Bool -> m ()
errorIfFailed message mf = do
    ok <- mf
    unless ok $ error message

generateWorkspaceXml :: [String] -> String -> State Cursor ()
generateWorkspaceXml projects activeProblem = do
    let makeProjectElem projectName = mkElem "Project" (makeAttribs projectName)
        makeAttribs projectName = [("Name", projectName),("Path", projectName ++ "/" ++ projectName ++ ".project")]
                             ++ [("Active", "Yes") | projectName == activeProblem]

    modifyFromJust $ findRight (isTag "Codelite_Workspace")
    removeChildren (isTag "project")

    errorIfFailed "BuildMatrix not found" $ goToChild ["BuildMatrix"]
    forM_ projects $ \projectName -> modify (insertLeft $ Elem $ makeProjectElem projectName)

    removeChildren (isTag "WorkspaceConfiguration")
    forM_ ["Debug", "Release"] $ \conf -> do
        errorIfFailed "Coudln't insert WorkspaceConfiguration" $
            insertLastChild $ Elem $ mkElem "WorkspaceConfiguration" [("Name", conf), ("Selected", "yes")]
        forM_ projects $ \projectName -> do
            errorIfFailed "Coudln't insert Project" $
                insertLastChild $ Elem $ mkElem "Project" [("Name", projectName), ("ConfigName", conf)]
            modifyFromJust parent -- go to WorkspaceConfiguration
        modifyFromJust parent -- go to BuildMatrix
    goToDocRoot

