{-# OPTIONS_GHC -Wall #-}
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks,
    UserHooks(confHook, cleanHook, hookedPrograms), )
import Distribution.Simple.BuildPaths (autogenComponentModulesDir)
import Distribution.Simple.Program (ConfiguredProgram, Program, ProgramDb,
    lookupProgram, runProgram, simpleProgram, )
import Distribution.Simple.Setup (ConfigFlags(configDistPref, configVerbosity),
    CleanFlags(cleanDistPref), Flag,
    configConfigurationsFlags, fromFlag, )
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose,
    getDirectoryContentsRecursive, notice, rawSystemExit, )

import Distribution.System (OS(Windows), buildOS, )
import Distribution.Types.BuildInfo (BuildInfo, extraLibDirs, extraLibs, )
import Distribution.Types.Executable (Executable(buildInfo))
import Distribution.Types.Flag (mkFlagName, lookupFlagAssignment)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription, packageDescription)
import Distribution.Types.HookedBuildInfo (HookedBuildInfo)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo,
    localPkgDescr, withAllTargetsInBuildOrder', )
import Distribution.Types.PackageDescription (PackageDescription(executables))
import Distribution.Types.TargetInfo (targetCLBI)
import Distribution.Verbosity (Verbosity)

import Codec.Archive.Zip (Archive, Entry, ZipOption(OptRecursive), eRelativePath, eUncompressedSize,
    addEntryToArchive, fromArchive, emptyArchive, readEntry)

import Control.Monad (unless, when)
import qualified Data.ByteString.Lazy as B
import Data.List (nub)
import Data.Maybe (fromMaybe)
import System.Directory (doesDirectoryExist, doesFileExist, makeAbsolute,
    removeFile, removeDirectoryRecursive, withCurrentDirectory)
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

getCmakeBuildDir :: Flag FilePath -> FilePath
getCmakeBuildDir distDir = fromFlag distDir </> "build" </> "cbuild"

getResourcesZipFile :: Flag FilePath -> FilePath
getResourcesZipFile distDir = fromFlag distDir </> "build" </> "init.zip"

lookupConfFlag :: ConfigFlags -> String -> Bool -> Bool
lookupConfFlag flags flagName defaultValue = fromMaybe defaultValue $
   lookupFlagAssignment (mkFlagName flagName) (configConfigurationsFlags flags)

autogenModules :: PackageDescription -> LocalBuildInfo -> Flag FilePath -> IO ()
autogenModules pkg lbi distDir = do
    let haskellModuleName = "Paths_CaideExt" -- Must match autogen-modules/other-modules in the cabal file!
        haskellSource = "module " ++ haskellModuleName ++ "(resourcesZipFilePath) where\n" ++
                        "resourcesZipFilePath :: String\n" ++
                        "resourcesZipFilePath = \"" ++ getResourcesZipFile distDir ++ "\"\n" -- TODO: Proper escape
    withAllTargetsInBuildOrder' pkg lbi $ \targetInfo -> do
        let componentLBI = targetCLBI targetInfo
            targetDir = autogenComponentModulesDir lbi componentLBI
        writeFile (targetDir </> (haskellModuleName ++ ".hs")) haskellSource


inlinerConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags
                -> IO LocalBuildInfo
inlinerConfHook (pkg, pbi) flags = do
  -- print flags
  lbi <- confHook simpleUserHooks (pkg, pbi) flags

  let verbosity = fromFlag (configVerbosity flags)
      debug = lookupConfFlag flags "debug" False
      cppinliner = lookupConfFlag flags "cppinliner" True
      cmakeBuildType  = if debug then "Debug" else "Release"
      distDir = configDistPref flags
      inlinerBuildDir = getCmakeBuildDir distDir
      zipFile = getResourcesZipFile distDir

  autogenModules (packageDescription pkg) lbi distDir

  lbi' <- if not cppinliner
      then return lbi
      else do
          createDirectoryIfMissingVerbose verbosity True inlinerBuildDir
          notice verbosity $ "Configuring C++ inliner in " ++ inlinerBuildDir ++ "..."
          inlinerBuildAbsDir <- makeAbsolute inlinerBuildDir
          inlinerSrcAbsDir <- makeAbsolute inlinerSrcDir
          withCurrentDirectory inlinerBuildAbsDir $ rawSystemExit verbosity "cmake" [inlinerSrcAbsDir,
                          "-G", "Unix Makefiles", "-DCMAKE_BUILD_TYPE=" ++ cmakeBuildType,
                          "-DCAIDE_USE_SYSTEM_CLANG=OFF"]

          -- We build the C++ inliner library and create the zip file with resources in configure hook.
          -- Doing this in the build hook is also possible, but would require overriding LocalBuildInfo
          -- in build hook on the fly. Also, these things are typically needed to be done just once, so
          -- it makes sense to do them in configure hook.

          notice verbosity "Building C++ inliner..."

          -- TODO: We'd ideally like to use the -j option given to cabal-install itself.
          -- Alternatively we could use a command-specific option like
          -- 'cabal build --make-option=-j4', but see
          -- https://github.com/haskell/cabal/issues/1380 for why this doesn't work.

          -- -j4 hangs in MinGW on 64bit windows
          let threadFlags = ["-j4" | buildOS /= Windows]
              makeOptions = threadFlags

          rawSystemExit verbosity "cmake" $ ["--build", inlinerBuildDir, "--target", "caideInliner", "--"] ++ makeOptions

          -- TODO: ideally, this list should be generated by CMake and consumed by cabal
          let addInlinerLibs bi = bi {
                  extraLibs = [ "caideInliner"
                              , "clangTooling"
                              , "clangFrontend"
                              , "clangDriver"
                              , "clangSerialization"
                              , "clangParse"
                              , "clangSema"
                              , "clangAnalysis"
                              , "clangRewrite"
                              , "clangEdit"
                              , "clangAST"
                              , "clangLex"
                              , "clangBasic"
                              , "LLVMProfileData"
                              , "LLVMOption"
                              , "LLVMMCParser"
                              , "LLVMMC"
                              , "LLVMBitReader"
                              , "LLVMCore"
                              , "LLVMBinaryFormat"
                              , "LLVMSupport"
                              ] ++ extraLibs bi,
                  extraLibDirs = [inlinerBuildDir, inlinerBuildDir </> "llvm" </> "lib"] ++ extraLibDirs bi
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
        then (nub . concat) <$> mapM getDirectoryContentsRecursive files
        else return files
    entries <- mapM (readEntry' opts) filesAndChildren
    let changeEntryPath e = e { eRelativePath = relPath ++ "/" ++ eRelativePath e }
    return $ foldr addEntryToArchive archive $ map changeEntryPath entries


-- Zip resources. The archive will be embedded into the executable.
zipResources :: FilePath -> Verbosity -> Bool -> IO ()
zipResources zipFile verbosity includeClangHeaders = do
    -- TODO: always recreate the archive if we do this in configure stage?
    zipFileExists <- doesFileExist zipFile
    unless zipFileExists $ do
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
                                inlinerSrcDir </> "clang" </> "lib" </> "Headers"
                B.writeFile zipFile $ fromArchive archive'


-- This hook doesn't seem to be called at all by cabal-install...
inlinerCleanHook :: PackageDescription -> () -> UserHooks -> CleanFlags -> IO ()
inlinerCleanHook pkg v hooks flags = do
    -- print flags
    let distDir = cleanDistPref flags
        buildDir = getCmakeBuildDir distDir
        resourcesZipFile = getResourcesZipFile distDir
    whenM (doesDirectoryExist buildDir) $ removeDirectoryRecursive buildDir
    whenM (doesFileExist resourcesZipFile) $ removeFile resourcesZipFile

    cleanHook simpleUserHooks pkg v hooks flags


whenM :: Monad m => m Bool -> m () -> m ()
whenM mcondition maction = do
    condition <- mcondition
    when condition maction

type Lifter a b = (a -> a) -> b -> b

onLocalPkgDescr :: Lifter PackageDescription LocalBuildInfo
onLocalPkgDescr f lbi = lbi { localPkgDescr = f (localPkgDescr lbi) }

onExecutables :: Lifter Executable PackageDescription
onExecutables f pd = pd { executables = map f (executables pd) }

onExeBuildInfo :: Lifter BuildInfo Executable
onExeBuildInfo f exe = exe { buildInfo = f (buildInfo exe) }

onLocalLibBuildInfo :: Lifter BuildInfo LocalBuildInfo
onLocalLibBuildInfo = onLocalPkgDescr . onExecutables . onExeBuildInfo

