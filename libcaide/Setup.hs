{-# OPTIONS_GHC -Wall #-}
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks,
    UserHooks(confHook, cleanHook, hookedPrograms), )
import Distribution.Simple.BuildPaths (autogenComponentModulesDir)
import Distribution.Simple.Program (Program, simpleProgram, )
import Distribution.Simple.Setup (ConfigFlags(configDistPref, configSharedLib, configVerbosity),
    CleanFlags(cleanDistPref), Flag,
    configConfigurationsFlags, fromFlag, toFlag,)
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose,
    getDirectoryContentsRecursive, notice, rawSystemExit, )

import Distribution.System (OS(Windows), Platform(Platform), buildOS, )
import Distribution.Types.BuildInfo (BuildInfo, extraLibDirs, extraLibs, )
import Distribution.Types.Flag (mkFlagName, lookupFlagAssignment)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription, packageDescription)
import Distribution.Types.HookedBuildInfo (HookedBuildInfo)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo(hostPlatform),
    localPkgDescr, withAllTargetsInBuildOrder', )
import Distribution.Types.Library (Library(libBuildInfo))
import Distribution.Types.PackageDescription (PackageDescription(library, subLibraries))
import Distribution.Types.TargetInfo (targetCLBI)
import Distribution.Utils.Path (getSymbolicPath, unsafeMakeSymbolicPath)
import Distribution.Verbosity (Verbosity)

import Codec.Archive.Zip (Archive, Entry, ZipOption(OptRecursive), eRelativePath, eUncompressedSize,
    addEntryToArchive, fromArchive, emptyArchive, readEntry)

import Control.Monad (unless, when)
import qualified Data.ByteString.Lazy as B
import Data.Foldable (for_)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import GHC.Conc (getNumProcessors)
import System.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute,
    copyFile, removeFile, withCurrentDirectory)
import System.Environment (getEnvironment)
import System.FilePath ((</>))


main :: IO ()
main =
  defaultMainWithHooks simpleUserHooks
      { confHook  = inlinerConfHook
      , cleanHook = inlinerCleanHook
      , hookedPrograms = [cmakeProgram] ++ hookedPrograms simpleUserHooks
      }

cmakeProgram :: Program
cmakeProgram = simpleProgram "cmake"

inlinerSrcDir :: FilePath
inlinerSrcDir = "cbits" </> "cpp-inliner" </> "src"

getCmakeBuildDir :: LocalBuildInfo -> FilePath
getCmakeBuildDir lbi = "cbits" </> "build" </> gccPrefix
  where
    Platform arch os = hostPlatform lbi
    gccPrefix = show os ++ "_" ++ show arch

-- getResourcesZipFile :: Flag FilePath -> FilePath
getResourcesZipFile distDir = getSymbolicPath (fromFlag distDir) </> "build" </> "init.zip"

lookupConfFlag :: ConfigFlags -> String -> Bool -> Bool
lookupConfFlag flags flagName defaultValue = fromMaybe defaultValue $
   lookupFlagAssignment (mkFlagName flagName) (configConfigurationsFlags flags)

-- autogenModules :: PackageDescription -> LocalBuildInfo -> Flag FilePath -> IO ()
autogenModules pkg lbi distDir = do
    let haskellModuleName = "Paths_CaideExt" -- Must match autogen-modules/other-modules in the cabal file!
        haskellSource = "module " ++ haskellModuleName ++ "(resourcesZipFilePath) where\n" ++
                        "resourcesZipFilePath :: String\n" ++
                        "resourcesZipFilePath = " ++ show (getResourcesZipFile distDir) ++ "\n"
    withAllTargetsInBuildOrder' pkg lbi $ \targetInfo -> do
        let componentLBI = targetCLBI targetInfo
            targetDir = getSymbolicPath $ autogenComponentModulesDir lbi componentLBI
        createDirectoryIfMissing True targetDir
        writeFile (targetDir </> (haskellModuleName ++ ".hs")) haskellSource


inlinerConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags
                -> IO LocalBuildInfo
inlinerConfHook (pkg, pbi) flags = do
    -- print flags
    let cppinliner = lookupConfFlag flags "cppinliner" True
        flags' = if cppinliner then flags{ configSharedLib = toFlag False } else flags
    lbi <- confHook simpleUserHooks (pkg, pbi) flags'

    let verbosity = fromFlag (configVerbosity flags')
        debug = lookupConfFlag flags' "debug" False
        cmakeBuildType  = if debug then "Debug" else "Release"
        distDir = configDistPref flags'
        inlinerBuildDir = getCmakeBuildDir lbi
        zipFile = getResourcesZipFile distDir

    autogenModules (packageDescription pkg) lbi distDir

    lbi' <- if not cppinliner
        then return lbi
        else do
            env <- getEnvironment
            -- TODO: We'd ideally like to use the -j option given to cabal-install itself by default.


            nproc <- getNumProcessors
            let defaultConfigureOptions = ["-G", if buildOS == Windows then "MinGW Makefiles" else "Unix Makefiles"]
                defaultBuildOptions = ["--", "-j" ++ show nproc]

                getOptions name defaultValue = case List.lookup name env of
                    Just val | val /= "" -> split ',' val
                    _ -> defaultValue
                -- TODO: instead of passing these options through environment variables, we could use
                -- something like 'cabal build --make-option=-j4', but see
                -- https://github.com/haskell/cabal/issues/1380 for why this doesn't work.
                cmakeConfigureArgs = getOptions "CAIDE_CMAKE_CONFIGURE_ARGS" defaultConfigureOptions
                cmakeBuildArgs = getOptions "CAIDE_CMAKE_BUILD_ARGS" defaultBuildOptions

            createDirectoryIfMissingVerbose verbosity True inlinerBuildDir
            inlinerBuildAbsDir <- makeAbsolute inlinerBuildDir
            unlessM (doesFileExist $ inlinerBuildAbsDir </> "CMakeCache.txt") $ do
                notice verbosity $ "Configuring C++ inliner in " ++ inlinerBuildDir ++ "..."
                inlinerSrcAbsDir <- makeAbsolute inlinerSrcDir
                withCurrentDirectory inlinerBuildAbsDir $ rawSystemExit verbosity Nothing "cmake" $ [
                                "-DCMAKE_BUILD_TYPE=" ++ cmakeBuildType,
                                "-DCAIDE_USE_SYSTEM_CLANG=OFF"] ++ cmakeConfigureArgs ++ [inlinerSrcAbsDir]

            -- We build the C++ inliner library and create the zip file with resources in configure hook.
            -- Doing this in the build hook is also possible, but would require overriding LocalBuildInfo
            -- in build hook on the fly. Also, these things are typically needed to be done just once, so
            -- it makes sense to do them in configure hook.

            notice verbosity "Building C++ inliner..."

            rawSystemExit verbosity Nothing "cmake" $ ["--build", inlinerBuildAbsDir, "--target", "caideInliner"] ++ cmakeBuildArgs

            -- Read the list of dependencies generated by CMake at configure stage.
            clangLibs <- List.delete "caideInliner" <$> split ';' <$> readFile (inlinerBuildAbsDir </> "caide-libs.txt")

            -- Rename the libraries, in case there is conflict with system-wide installed clang/llvm.
            let renamedClangLibs = map ("caide" ++) $ List.delete "caideInliner" clangLibs
                staticLibFileName libName = "lib" ++ libName ++ ".a"
            for_ (zip clangLibs renamedClangLibs) $ \(lib, renamedLib) ->
                copyFile
                (inlinerBuildAbsDir </> "llvm-project" </> "llvm" </> "lib" </> staticLibFileName lib)
                (inlinerBuildAbsDir </> staticLibFileName renamedLib)

            let addInlinerLibs bi = bi {
                    extraLibs = ["caideInliner"] ++ renamedClangLibs ++ extraLibs bi,
                    extraLibDirs = [unsafeMakeSymbolicPath inlinerBuildAbsDir] ++ extraLibDirs bi
                }

            return $ onLocalLibBuildInfo addInlinerLibs lbi

    zipResources zipFile verbosity cppinliner
    return lbi'

-- A strict version of readEntry
readEntry' :: [ZipOption] -> FilePath -> IO Entry
readEntry' opts path = do
    e <- readEntry opts path
    eUncompressedSize e `seq` return e

-- A version of addFilesToArchive that:
--   1. uses a strict version of readEntry
--   2. allows specifying a relative path of added files
addFilesToArchive' :: [ZipOption] -> Archive -> [FilePath] -> FilePath -> IO Archive
addFilesToArchive' opts archive files relPath = do
    filesAndChildren <- if OptRecursive `elem` opts
        then (List.nub . concat) <$> mapM getDirectoryContentsRecursive files
        else return files
    entries <- mapM (readEntry' opts) filesAndChildren
    let changeEntryPath e = e { eRelativePath = relPath ++ "/" ++ eRelativePath e }
    return $ foldr (addEntryToArchive . changeEntryPath) archive entries


-- Zip resources. The archive will be embedded into the executable.
zipResources :: FilePath -> Verbosity -> Bool -> IO ()
zipResources zipFile verbosity includeClangHeaders = do
    let addFilesToZipFile :: Archive -> FilePath -> FilePath -> IO Archive
        addFilesToZipFile archive relPath filesPath = withCurrentDirectory filesPath $
            addFilesToArchive' [OptRecursive] archive ["."] relPath

    notice verbosity "Zipping resource files..."
    archive <- addFilesToZipFile emptyArchive "." $ "res" </> "init"
    if not includeClangHeaders
        then B.writeFile zipFile $ fromArchive archive
        else do
            let clangBuiltinsDir = "include" </> "clang-builtins"
            createDirectoryIfMissingVerbose verbosity True clangBuiltinsDir
            archive' <- addFilesToZipFile archive clangBuiltinsDir $
                            inlinerSrcDir </> "llvm-project" </> "clang" </> "lib" </> "Headers"
            B.writeFile zipFile $ fromArchive archive'


-- This hook doesn't seem to be called at all by cabal-install...
inlinerCleanHook :: PackageDescription -> () -> UserHooks -> CleanFlags -> IO ()
inlinerCleanHook pkg v hooks flags = do
    -- print flags
    let distDir = cleanDistPref flags
        resourcesZipFile = getResourcesZipFile distDir
    whenM (doesFileExist resourcesZipFile) $ removeFile resourcesZipFile

    cleanHook simpleUserHooks pkg v hooks flags


whenM :: Monad m => m Bool -> m () -> m ()
whenM mcondition maction = do
    condition <- mcondition
    when condition maction

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mcondition maction = do
    condition <- mcondition
    unless condition maction

split :: Eq a => a -> [a] -> [[a]]
split _sep [] = []
split sep s = prefix: split sep (tailIfNonEmpty rest)
  where
    (prefix, rest) = List.break (== sep) s
    tailIfNonEmpty = drop 1

type Lifter a b = (a -> a) -> b -> b

onLocalPkgDescr :: Lifter PackageDescription LocalBuildInfo
onLocalPkgDescr f lbi = lbi { localPkgDescr = f (localPkgDescr lbi) }

onLibraries :: Lifter Library PackageDescription
onLibraries f pd = case library pd' of
    Nothing -> pd'
    Just lib -> pd' { library = Just (f lib) }
  where
    pd' = pd { subLibraries = map f (subLibraries pd) }

onLibBuildInfo :: Lifter BuildInfo Library
onLibBuildInfo f lib = lib { libBuildInfo = f (libBuildInfo lib) }

onLocalLibBuildInfo :: Lifter BuildInfo LocalBuildInfo
-- onLocalLibBuildInfo = onLocalPkgDescr . onExecutables . onExeBuildInfo
onLocalLibBuildInfo = onLocalPkgDescr . onLibraries . onLibBuildInfo

