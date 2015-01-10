module Caide.Features.VisualStudio (
      feature
) where

import Prelude hiding (readFile)

import Control.Applicative ((<$>))
import Control.Monad (forM_, forM, when, unless)
import Control.Monad.State.Strict (execState, evalState, State, get)
import Control.Monad.State (liftIO)
import Data.Char (toUpper, ord)
import Data.List.Utils (replace)

import qualified Data.Text as T
import Data.Text.IO (readFile)

import Data.UUID (fromString, toString, UUID)
import Data.UUID.V5 (generateNamed)

import Filesystem (isFile, writeTextFile, listDirectory, copyFile)
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import Filesystem.Path ((</>), basename)

import Text.XML.Light (parseXML, Content(..))
import Text.XML.Light.Cursor

import Caide.Configuration (readProblemConfig, getActiveProblem)
import Caide.Types
import Caide.Util (copyFileToDir)
import Caide.Xml (removeChildren, isTag, insertLastChild, mkElem, mkText, modifyFromJust,
                  getTextContent, hasAttr, goToDocRoot, showXml, goToChild, hasAttrEqualTo)


caideUuid :: UUID
Just caideUuid = fromString "CA1DE777-8B4A-11D0-8D11-00A0C91BC942"

getGuidForProblem :: ProblemID -> String
getGuidForProblem = map toUpper . toString . generateNamed caideUuid . map (fromIntegral . ord)


-- | Deprecated: use VsCaide extension instead
feature :: Feature
feature  = noOpFeature
    { onProblemCodeCreated = generateProject
    , onProblemCheckedOut  = const generateSolution
    }


generateProject :: ProblemID -> CaideIO ()
generateProject probId = do
    croot <- caideRoot
    hProblem <- readProblemConfig probId
    lang <- getProp hProblem "problem" "language"
    when (lang `elem` ["simplecpp", "cpp", "c++"]) $ do
        liftIO $ do
            putStrLn "Generating VS project"
            let problemDir  = croot </> decodeString probId
                projectFile = problemDir </> decodeString (probId ++ ".vcxproj")
                userFile    = problemDir </> decodeString (probId ++ ".vcxproj.user")
            projectExists <- isFile projectFile
            if projectExists
            then putStrLn $ probId ++ ".vcxproj already exists. Not overwriting."
            else do
                xmlString <- readFile . encodeString $ croot </> decodeString "templates" </> decodeString "vs2012_template.vcxproj"
                let doc = parseXML xmlString
                    Just cursor = fromForest doc
                    transformed = execState (generateProjectXML probId) cursor
                    outXml = T.pack . replace "&#39;" "'" . showXml $ transformed
                    templateUserFile = croot </> decodeString "templates" </> decodeString "vs2012_template.vcxproj.user"
                outXml `seq` writeTextFile projectFile outXml
                copyFile templateUserFile userFile
                putStrLn $ probId ++ ".vcxproj for Visual Studio successfully generated."
        generateSolution


generateProjectXML :: ProblemID -> State Cursor ()
generateProjectXML probId = do
    modifyFromJust $ findRight (isTag "Project")
    removeChildren $ \c -> isTag "ItemGroup" c && not (hasAttr "Label" c)

    errorIfFailed "Couldn't insert ItemGroup element" $
        insertLastChild $ Elem $ mkElem "ItemGroup" []
    forM_ [probId ++ ".cpp", probId ++ "_test.cpp"] $ \file -> do
        errorIfFailed "Couldn't insert ClCompile element" $
            insertLastChild $ Elem $ mkElem "ClCompile" [("Include", file)]
        modifyFromJust parent
    modifyFromJust parent

    removeChildren $ \c -> isTag "PropertyGroup" c && hasAttrEqualTo "Label" "Globals" c
    errorIfFailed "Couldn't insert PropertyGroup" $
         insertLastChild $ Elem $ mkElem "PropertyGroup" [("Label", "Globals")]


    errorIfFailed "Couldn't insert ProjectGuid element" $
        insertLastChild $ Elem $ mkElem "ProjectGuid" []
    errorIfFailed "Couldn't insert ProjectGuid element" $
        insertLastChild $ mkText $ "{" ++ getGuidForProblem probId ++ "}"
    modifyFromJust parent
    modifyFromJust parent


    errorIfFailed "Couldn't insert RootNamespace element" $
        insertLastChild $ Elem $ mkElem "RootNamespace" []
    errorIfFailed "Couldn't insert RootNamespace element" $
        insertLastChild $ mkText probId

    goToDocRoot


generateSolution :: CaideIO ()
generateSolution = do
    problems <- getVSProblems
    activeProblem <- getActiveProblem
    croot <- caideRoot
    let workspaceFile = croot </> decodeString "caide.sln"
        sln = generateSolutionFileContents problems activeProblem
        vspropsFile = croot </> decodeString "vs_common.props"
        vspropsTemplate = croot </> decodeString "templates" </> decodeString "vs_common.props"
    liftIO $ do
        writeTextFile workspaceFile . T.pack $ sln
        propsExists <- isFile vspropsFile
        unless propsExists $ copyFileToDir vspropsTemplate croot


getVSProblems :: CaideIO [(ProblemID, String)]
getVSProblems = do
    croot <- caideRoot
    liftIO $ do
        dirs <- listDirectory croot
        let problemIds = map (encodeString . basename) dirs
        guids <- forM problemIds $ \probId -> do
            let projectFile = croot </> decodeString probId </> decodeString (probId ++ ".vcxproj")
            haveProject <- isFile projectFile
            if haveProject
                then do
                    xmlString <- readFile . encodeString $ projectFile
                    let doc = parseXML xmlString
                        Just cursor = fromForest doc
                    return $ evalState getProjectGuid cursor
                else return Nothing

        return [(probId, guid) | (probId, Just guid) <- zip problemIds guids]


getProjectGuid :: State Cursor (Maybe String)
getProjectGuid = do
    modifyFromJust $ findRight (isTag "Project")
    modifyFromJust $ findChild $ \c -> isTag "PropertyGroup" c && hasAttrEqualTo "Label" "Globals" c
    errorIfFailed "Couldn't find project GUID" $ goToChild ["ProjectGuid"]
    getTextContent <$> get

errorIfFailed :: Monad m => String -> m Bool -> m ()
errorIfFailed message mf = do
    ok <- mf
    unless ok $ error message

generateSolutionFileContents :: [(String, String)] -> String -> String
generateSolutionFileContents problems _activeProblem = unlines $
        ["Microsoft Visual Studio Solution File, Format Version 12.00",
         "# Visual Studio 2012"
        ]

        ++ concatMap makeProjectDefinition problems

        ++ [
        "Global",
        "\tGlobalSection(SolutionConfigurationPlatforms) = preSolution",
        "\t\tDebug|Win32 = Debug|Win32",
        "\t\tRelease|Win32 = Release|Win32",
        "\tEndGlobalSection",
        "\tGlobalSection(ProjectConfigurationPlatforms) = postSolution"
        ]

        ++ concatMap makeProjectConfiguration problems

        ++ [
        "\tEndGlobalSection",
        "\tGlobalSection(SolutionProperties) = preSolution",
        "\t\tHideSolutionNode = FALSE",
        "\tEndGlobalSection",
        "EndGlobal"
        ]
    where
        solutionGuid = map toUpper . toString $ caideUuid
        makeProjectDefinition (probId, probGuid) = ["Project(" ++ showGuid solutionGuid ++
                ") = \"" ++ probId ++ "\", \"" ++ probId ++ "\\" ++ probId ++ ".vcxproj\", " ++
                probGuid,
            "EndProject"]
        makeProjectConfiguration (_, guid) = do
            conf <- ["Debug|Win32", "Release|Win32"]
            tag <- ["ActiveCfg", "Build.0"]
            return $ "\t\t" ++ guid ++ "." ++ conf ++ "." ++ tag ++ " = " ++ conf
        showGuid guid = "\"{" ++ guid ++ "}\""

