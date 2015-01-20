{-# LANGUAGE OverloadedStrings #-}

module Caide.Features.Codelite (
      feature
) where


import Control.Applicative ((<$>))
import Control.Monad (forM_, when, unless)
import Control.Monad.State.Strict (execState, modify, State)
import Control.Monad.State (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Filesystem (isFile, writeTextFile, listDirectory, createDirectory)
import Filesystem.Path.CurrentOS (fromText, encodeString)
import Filesystem.Path ((</>), basename, hasExtension)

import Text.XML.Light (parseXML, Content(..))
import Text.XML.Light.Cursor

import Caide.Types
import Caide.Xml (goToChild, removeChildren, isTag, insertLastChild, mkElem, modifyFromJust,
                  changeAttr, hasAttrEqualTo, goToDocRoot, showXml, forEachChild)
import Caide.Configuration (readProblemState, getActiveProblem)
import Caide.Util (listDir, pathToText)

feature :: Feature
feature =  noOpFeature
    { onProblemCodeCreated = generateProject
    , onProblemCheckedOut = \probId -> generateWorkspace >> generateProject probId
    , onProblemRemoved = const generateWorkspace
    }

generateProject :: ProblemID -> CaideIO ()
generateProject probId = do
    hProblem <- readProblemState probId
    lang <- getProp hProblem "problem" "language"
    when (lang `elem` ["simplecpp", "cpp", "c++" :: T.Text]) $ do
        croot <- caideRoot

        let projectFile = croot </> fromText probId </> fromText (T.append probId ".project")
            needLibrary = lang `elem` ["cpp", "c++"]
            libProjectDir  = croot </> "cpplib"
            libProjectFile = libProjectDir </> "cpplib.project"

        when needLibrary $ liftIO $ do
            libProjectExists <- isFile libProjectFile
            -- TODO update files included into cpplib.project when it already exists
            unless libProjectExists $ do
                createDirectory True libProjectDir
                xmlString <- readFile . encodeString $ croot </> "templates" </> "codelite_project_template.project"
                allFiles <- fst <$> listDir libProjectDir
                let doc = parseXML xmlString
                    Just cursor = fromForest doc
                    files = map pathToText . filter (\f -> f `hasExtension` "h" || f `hasExtension` "cpp") $ allFiles
                    includePaths = ["."]
                    libs = []
                    libraryPaths = []
                    transformed = execState (generateProjectXML "cpplib" "Static Library" files includePaths libraryPaths libs) cursor
                transformed `seq` writeTextFile libProjectFile . T.pack . showXml $ transformed
                putStrLn "cpplib.project for Codelite successfully generated."

        -- TODO generate submission.project
        projectExists <- liftIO $ isFile projectFile
        if projectExists
        then liftIO . T.putStrLn . T.concat $ [probId, ".project already exists. Not overwriting."]
        else do
            liftIO $ do
                putStrLn "Generating codelite project"
                xmlString <- readFile . encodeString $ croot </> "templates" </> "codelite_project_template.project"
                let doc = parseXML xmlString
                    Just cursor = fromForest doc
                    files = map (T.append probId) [".cpp", "_test.cpp"]
                    includePaths = "." : ["../cpplib" | needLibrary]
                    libs = ["cpplib" | needLibrary]
                    libraryPaths = ["../cpplib/$(ConfigurationName)" | needLibrary]
                    transformed = execState (generateProjectXML probId "Executable" files includePaths libraryPaths libs) cursor
                transformed `seq` writeTextFile projectFile . T.pack . showXml $ transformed
                T.putStrLn . T.concat $ [probId, ".project for Codelite successfully generated."]
            generateWorkspace

setProjectName :: T.Text -> State Cursor Bool
setProjectName projectName = do
    goToDocRoot
    modifyFromJust $ findRight (isTag "CodeLite_Project")
    changeAttr "Name" $ T.unpack projectName

setProjectType :: String -> State Cursor Bool
setProjectType projectType = do
    goToDocRoot
    modifyFromJust $ findRight (isTag "CodeLite_Project")
    errorIfFailed "Couldn't find Settings node" $ goToChild ["Settings"]
    changed <- changeAttr "Type" projectType
    confChanged <- forEachChild (isTag "Configuration") $ changeAttr "Type" projectType
    return $ or (changed:confChanged)

generateProjectXML :: T.Text -> String -> [T.Text] -> [T.Text] -> [T.Text] -> [T.Text] -> State Cursor ()
generateProjectXML projectName projectType sourceFiles includePaths libPaths libs = do
    _ <- setProjectName projectName
    _ <- setProjectType projectType
    goToDocRoot
    modifyFromJust $ findRight (isTag "Codelite_Project")
    modifyFromJust $ findChild $ \c -> isTag "VirtualDirectory" c && hasAttrEqualTo "Name" "src" c
    removeChildren $ isTag "File"
    forM_ sourceFiles $ \file -> do
         errorIfFailed "Couldn't insert File element" $
            insertLastChild $ Elem $ mkElem "File" [("Name", T.unpack file)]
         modifyFromJust parent

    modifyFromJust parent
    errorIfFailed "Couldn't find GlobalSettings/Compiler node" $
        goToChild ["Settings", "GlobalSettings", "Compiler"]

    forM_ includePaths $ \path -> do
        errorIfFailed "Couldn't insert IncludePath tag" $
            insertLastChild $ Elem $ mkElem "IncludePath" [("Value", T.unpack path)]
        modifyFromJust parent -- <Compiler>
    modifyFromJust parent -- <GlobalSettings>

    errorIfFailed "Couldn't find GlobalSettings/Linker node" $
        goToChild ["Linker"]
    forM_ libPaths $ \libPath -> do
        errorIfFailed "Couldn't insert LibraryPath tag" $
            insertLastChild $ Elem $ mkElem "LibraryPath" [("Value", T.unpack libPath)]
        modifyFromJust parent -- <Linker>
    forM_ libs $ \lib -> do
        errorIfFailed "Couldn't insert Library tag" $
            insertLastChild $ Elem $ mkElem "Library" [("Value", T.unpack lib)]
        modifyFromJust parent -- <Linker>


    goToDocRoot

generateWorkspace :: CaideIO ()
generateWorkspace = do
    croot <- caideRoot
    projects <- getCodeliteProjects
    activeProblem <- getActiveProblem
    let workspaceFile = croot </> "caide.workspace"

    liftIO $ do
        workspaceExists <- isFile workspaceFile
        let existingWorkspace = if workspaceExists
            then workspaceFile
            else croot </> "templates" </> "codelite_workspace_template.workspace"
        xmlString <- readFile $ encodeString existingWorkspace
        let doc = parseXML xmlString
            Just cursor = fromForest doc
            transformed = execState (generateWorkspaceXml projects activeProblem) cursor
        transformed `seq` writeTextFile workspaceFile . T.pack . showXml $ transformed

-- Includes problems and CPP library
getCodeliteProjects :: CaideIO [T.Text]
getCodeliteProjects = do
    croot <- caideRoot
    liftIO $ do
        dirs <- listDirectory croot
        let problemIds = map (pathToText . basename) dirs
            haveCodelite probId = isFile $ croot </> fromText probId </> fromText (T.append probId ".project")
        projectExists <- mapM haveCodelite problemIds
        return [probId | (probId, True) <- zip problemIds projectExists]


errorIfFailed :: Monad m => String -> m Bool -> m ()
errorIfFailed message mf = do
    ok <- mf
    unless ok $ error message

generateWorkspaceXml :: [T.Text] -> T.Text -> State Cursor ()
generateWorkspaceXml projects activeProblem = do
    let makeProjectElem projectName = mkElem "Project" (makeAttribs $ T.unpack projectName)
        makeAttribs projectName = [("Name", projectName),("Path", projectName ++ "/" ++ projectName ++ ".project")]
                             ++ [("Active", "Yes") | projectName == T.unpack activeProblem]

    modifyFromJust $ findRight (isTag "Codelite_Workspace")
    removeChildren (isTag "project")

    errorIfFailed "BuildMatrix tag not found" $ goToChild ["BuildMatrix"]
    forM_ projects $ \projectName -> modify (insertLeft $ Elem $ makeProjectElem projectName)

    removeChildren (isTag "WorkspaceConfiguration")
    forM_ ["Debug", "Release"] $ \conf -> do
        errorIfFailed "Couldn't insert WorkspaceConfiguration tag" $
            insertLastChild $ Elem $ mkElem "WorkspaceConfiguration" [("Name", conf), ("Selected", "yes")]
        forM_ projects $ \projectName -> do
            errorIfFailed "Couldn't insert Project tag" $
                insertLastChild $ Elem $ mkElem "Project" [("Name", T.unpack projectName), ("ConfigName", conf)]
            modifyFromJust parent -- go to WorkspaceConfiguration
        modifyFromJust parent -- go to BuildMatrix
    goToDocRoot

