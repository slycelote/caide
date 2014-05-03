module Caide.Features.Codelite (
      feature
) where

import Prelude hiding (readFile)

import Control.Monad (forM_, when, unless)
import Control.Monad.State.Strict (execState, modify, State)
import qualified Data.Text as T
import Data.Text.IO (readFile)

import Filesystem (isFile, writeTextFile, listDirectory)
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import Filesystem.Path ((</>), basename)

import Text.XML.Light (parseXML, Content(..),)
import Text.XML.Light.Cursor

import Caide.Types
import Caide.Xml (goToChild, removeChildren, isTag, insertLastChild, mkElem, modifyFromJust,
                  changeAttr, hasAttr, goToDocRoot, showXml)
import Caide.Configuration (readProblemConfig, getProblemOption, getProblemConfigFile, getActiveProblem)

feature :: Feature
feature = Feature
    { onProblemCreated = \_ _ -> return ()
    , onProblemCodeCreated = generateProject
    , onProblemCheckedOut = \env _ -> generateWorkspace env
    }

generateProject :: CaideEnvironment -> ProblemID -> IO ()
generateProject env probId = do
    putStrLn "Generating codelite project"
    conf <- readProblemConfig $ getProblemConfigFile env probId
    when (getProblemOption conf "problem" "language" `elem` ["simplecpp"]) $ do
        let projectFile = getRootDirectory env </> decodeString probId </> decodeString (probId ++ ".project")
        projectExists <- isFile projectFile
        if projectExists
        then putStrLn $ probId ++ ".project already exists. Not overwriting."
        else do
            xmlString <- readFile . encodeString $ getRootDirectory env </> decodeString "templates" </> decodeString "codelite_project_template.project"
            let doc = parseXML xmlString
                Just cursor = fromForest doc
                transformed = execState (generateProjectXML probId) cursor
            transformed `seq` writeTextFile projectFile . T.pack . showXml $ transformed
            putStrLn $ probId ++ ".project for Codelite successfully generated."
            generateWorkspace env

generateProjectXML :: ProblemID -> State Cursor ()
generateProjectXML probId = do
    modifyFromJust $ findRight (isTag "Codelite_Project")
    changeAttr "Name" probId
    modifyFromJust $ findChild $ \c -> isTag "VirtualDirectory" c && hasAttr "Name" "src" c
    removeChildren (isTag "File")
    forM_ [probId ++ ".cpp", probId ++ "_test.cpp"] $ \file -> do
         ok <- insertLastChild $ Elem $ mkElem "File" [("Name", file)]
         unless ok $ error "Couldn't insert File element"
         modifyFromJust parent
    goToDocRoot

generateWorkspace :: CaideEnvironment -> IO ()
generateWorkspace env = do
    problems <- getCodeliteProblems env
    activeProblem <- getActiveProblem env
    let workspaceFile = getRootDirectory env </> decodeString "caide.workspace"
    workspaceExists <- isFile workspaceFile
    let existingWorkspace = if workspaceExists
        then workspaceFile
        else getRootDirectory env </> decodeString "templates" </> decodeString "codelite_workspace_template.workspace"
    xmlString <- readFile $ encodeString existingWorkspace
    let doc = parseXML xmlString
        Just cursor = fromForest doc
        transformed = execState (generateWorkspaceXml problems activeProblem) cursor
    transformed `seq` writeTextFile workspaceFile . T.pack . showXml $ transformed

getCodeliteProblems :: CaideEnvironment -> IO [ProblemID]
getCodeliteProblems env = do
    let caideRoot = getRootDirectory env
    dirs <- listDirectory caideRoot
    let problemIds = map (encodeString . basename) dirs
        haveCodelite probId = isFile $ caideRoot </> decodeString probId </> decodeString (probId ++ ".project")
    projectExists <- mapM haveCodelite problemIds
    return [probId | (probId, True) <- zip problemIds projectExists]

generateWorkspaceXml :: [String] -> String -> State Cursor ()
generateWorkspaceXml problems activeProblem = do
    let makeProjectElem probId = mkElem "Project" (makeAttribs probId)
        makeAttribs probId = [("Name", probId),("Path", probId ++ "/" ++ probId ++ ".project")]
                             ++ [("Active", "Yes") | probId == activeProblem]
    modifyFromJust $ findRight (isTag "Codelite_Workspace")
    removeChildren (isTag "project")
    ok <- goToChild ["BuildMatrix"]
    unless ok $ error "BuildMatrix not found"
    forM_ problems $ \probId -> modify (insertLeft $ Elem $ makeProjectElem probId)
    removeChildren (isTag "WorkspaceConfiguration")
    forM_ ["Debug", "Release"] $ \conf -> do
        ok <- insertLastChild $ Elem $ mkElem "WorkspaceConfiguration" [("Name", conf), ("Selected", "yes")]
        unless ok $ error "Coudln't insert WorkspaceConfiguration"
        forM_ problems $ \probId -> do
            ok <- insertLastChild $ Elem $ mkElem "Project" [("Name", probId), ("ConfigName", conf)]
            unless ok $ error "Coudln't insert Project"
            modifyFromJust parent -- go to WorkspaceConfiguration
        modifyFromJust parent -- go to BuildMatrix
    goToDocRoot
