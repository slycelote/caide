import Codec.Archive.Zip
import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.Char(isSpace)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Distribution.PackageDescription
import Distribution.Verbosity
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.Db
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.System(OS(..), buildOS)
import System.Directory hiding(canonicalizePath)
import System.Environment(getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath
import System.Info(os)
import System.IO.Error(isDoesNotExistError)
import System.Process (readProcessWithExitCode)


main :: IO ()
main =
  defaultMainWithHooks simpleUserHooks
      { confHook  = inlinerConfHook
      , buildHook = inlinerBuildHook
      , cleanHook = inlinerCleanHook
      , hookedPrograms = [ cmakeProgram
                         , makeProgram
                         ] ++ hookedPrograms simpleUserHooks
      }


canonicalizePath :: FilePath -> IO FilePath
canonicalizePath path = do
  canonicalPath <- inDir path $
    rawSystemStdout silent "sh" ["-c", "pwd"]
  return $ reverse . dropWhile isSpace . reverse . dropWhile isSpace $ canonicalPath


inlinerConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags
                 -> IO LocalBuildInfo
inlinerConfHook (pkg, pbi) flags = do
  let verbosity = fromFlag (configVerbosity flags)
      lookupConfFlag flagName defaultValue = fromMaybe defaultValue $
            lookup (FlagName flagName) (configConfigurationsFlags flags)
      debug = lookupConfFlag "debug" False
      cppinliner = lookupConfFlag "cppinliner" True

  lbi <- confHook simpleUserHooks (pkg, pbi) flags

  when cppinliner $ do
      -- Compute some paths that need to be absolute.
      curDir <- getCurrentDirectory
      let inlinerSrcDir    = curDir </> "cbits" </> "cpp-inliner" </> "src"
          inlinerBuildDir  = curDir </> "cbits" </> "build"
          cmakeBuildType   = if debug then "Debug" else "Release"

      createDirectoryIfMissingVerbose verbosity True inlinerBuildDir

      [inlinerSrcDirCanonical, inlinerBuildDirCanonical] <-
          mapM canonicalizePath [inlinerSrcDir, inlinerBuildDir]

      let cmakeOptions = ["-G", "Unix Makefiles", "-DCMAKE_BUILD_TYPE=" ++ cmakeBuildType,
                          "-DCAIDE_USE_SYSTEM_CLANG=OFF", "-DCAIDE_SHARED_LIBRARY=OFF",
                          inlinerSrcDirCanonical]

      notice verbosity "Configuring C++ inliner..."

      env <- getEnvironment
      inDir inlinerBuildDirCanonical $
          rawSystemExitWithEnv verbosity "cmake" cmakeOptions env

  return lbi


-- A strict version of readEntry
readEntry' :: [ZipOption] -> FilePath -> IO Entry
readEntry' opts path = do
    e <- readEntry opts path
    eUncompressedSize e `seq` return e

-- A version of addFilesToArchive that uses a strict version of readEntry
addFilesToArchive' :: [ZipOption] -> Archive -> [FilePath] -> IO Archive
addFilesToArchive' opts archive files = do
    filesAndChildren <- if OptRecursive `elem` opts
        then (nub . concat) <$> mapM getDirectoryContentsRecursive files
        else return files
    entries <- mapM (readEntry' opts) filesAndChildren
    return $ foldr addEntryToArchive archive entries


-- Zip resources. The archive will be embedded into the executable.
zipResources :: FilePath -> Verbosity -> Maybe FilePath -> IO ()
zipResources curDir verbosity inlinerSrcDir = do
    let initFile = curDir </> "res" </> "init.zip"
    zipFileExists <- doesFileExist initFile
    unless zipFileExists $ do
        notice verbosity "Zipping resource files..."

        let addFilesToZipFile :: Archive -> FilePath -> IO Archive
            addFilesToZipFile archive filesPath = inDir filesPath $
                addFilesToArchive' [OptRecursive] archive ["."]

        archive <- addFilesToZipFile emptyArchive $ curDir </> "res" </> "init"
        case inlinerSrcDir of
            Nothing -> B.writeFile initFile $ fromArchive archive
            Just dir -> do
                archive' <- addFilesToZipFile archive $ dir </> "clang" </> "lib" </> "Headers"
                B.writeFile initFile $ fromArchive archive'


inlinerBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
inlinerBuildHook pkg lbi usrHooks flags = do
  let verbosity = fromFlag (buildVerbosity flags)
      lookupConfFlag flagName defaultValue = fromMaybe defaultValue $
            lookup (FlagName flagName) (configConfigurationsFlags $ configFlags lbi)
      debug = lookupConfFlag "debug" False
      cppinliner = lookupConfFlag "cppinliner" True
  curDir <- getCurrentDirectory

  -- Build C++ library, if necessary
  if cppinliner
     then do
        let inlinerSrcDir    = curDir </> "cbits" </> "cpp-inliner" </> "src"
            inlinerBuildDir  = curDir </> "cbits" </> "build"
            -- TODO: ideally, this list should be generated by CMake and consumed by cabal
            addInlinerLibs bi = bi {
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
                            , "LLVMOption"
                            , "LLVMMCParser"
                            , "LLVMMC"
                            , "LLVMBitReader"
                            , "LLVMCore"
                            , "LLVMSupport"
                            ] ++ extraLibs bi
            }
            lbi' = onLocalLibBuildInfo addInlinerLibs lbi

        notice verbosity "Building C++ inliner..."

        -- TODO: We'd ideally like to use the -j option given to cabal-install itself.
        -- Alternatively we could use a command-specific option like
        -- 'cabal install --make-option=-j4', but see
        -- https://github.com/haskell/cabal/issues/1380 for why this doesn't work.

        -- -j4 hangs in MinGW on 64bit windows
        let threadFlags = ["-j4" | buildOS /= Windows]
            makeOptions = threadFlags ++ ["caideInliner"]

        inDir inlinerBuildDir $ do
          -- Workaround for 'make not found' in MinGW
          buildMade <- case buildOS of
              Windows -> do
                (exitCode, _, _) <- readProcessWithExitCode "make" makeOptions ""
                return $ exitCode == ExitSuccess
              _       -> return False

          env <- getEnvironment
          unless buildMade $
            rawSystemExitWithEnv verbosity "make" makeOptions env

        zipResources curDir verbosity $ Just inlinerSrcDir

        -- Build Haskell code
        buildHook simpleUserHooks (localPkgDescr lbi') lbi' usrHooks flags

      else do
        -- No cppinliner flag
        zipResources curDir verbosity Nothing
        buildHook simpleUserHooks (localPkgDescr lbi) lbi usrHooks flags


inlinerCleanHook :: PackageDescription -> () -> UserHooks -> CleanFlags -> IO ()
inlinerCleanHook pkg v hooks flags = do
    curDir <- getCurrentDirectory
    let verbosity = fromFlag (cleanVerbosity flags)
        buildDir = curDir </> "cbits" </> "build"
        resourcesZipFile = curDir </> "res" </> "init.zip"
    buildDirExists <- doesDirectoryExist buildDir
    when buildDirExists $ removeDirectoryRecursive buildDir
    resourcesZipFileExists <- doesFileExist resourcesZipFile
    when resourcesZipFileExists $ removeFile resourcesZipFile

    cleanHook simpleUserHooks pkg v hooks flags


makeProgram, cmakeProgram :: Program
makeProgram    = simpleProgram "make"
cmakeProgram   = simpleProgram "cmake"

inDir :: FilePath -> IO a -> IO a
inDir dir act = do
    curDir <- getCurrentDirectory
    bracket_ (setCurrentDirectory dir)
             (setCurrentDirectory curDir)
             act

type Lifter a b = (a -> a) -> b -> b

onLocalPkgDescr :: Lifter PackageDescription LocalBuildInfo
onLocalPkgDescr f lbi = lbi { localPkgDescr = f (localPkgDescr lbi) }

onExecutables :: Lifter Executable PackageDescription
onExecutables f pd = pd { executables = map f (executables pd) }

onExeBuildInfo :: Lifter BuildInfo Executable
onExeBuildInfo f exe = exe { buildInfo = f (buildInfo exe) }

onLocalLibBuildInfo :: Lifter BuildInfo LocalBuildInfo
onLocalLibBuildInfo = onLocalPkgDescr . onExecutables . onExeBuildInfo

onPrograms :: Lifter ProgramDb LocalBuildInfo
onPrograms f lbi = lbi { withPrograms = f (withPrograms lbi) }

