{-# LANGUAGE TupleSections #-}

import Codec.Archive.Zip
import Control.Exception
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import Data.Char(isSpace)
import Data.List (isPrefixOf, intersperse, nub)
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX ( utcTimeToPOSIXSeconds )
import Distribution.PackageDescription
import Distribution.Verbosity
import Distribution.Simple
import Distribution.Simple.BuildPaths
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.Simple.Program
import Distribution.Simple.Program.Db
--import Distribution.Simple.Program.Find
import Distribution.System(OS(..), buildOS)
import System.Directory hiding(canonicalizePath)
import System.Environment(getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath
import System.Info(os)
import System.IO.Error(isDoesNotExistError)
import System.Process (readProcessWithExitCode)

main :: IO ()
main = do
  curDir <- getCurrentDirectory

  defaultMainWithHooks simpleUserHooks
      { confHook  = libClangConfHook
      , buildHook = libClangBuildHook
      , cleanHook = libClangCleanHook

      , hookedPrograms = [ confProgram
                         , makeProgram
                         , swVersProgram
                         ] ++ hookedPrograms simpleUserHooks
      }


canonicalizePath :: FilePath -> IO FilePath
canonicalizePath path = do
  canonicalPath <- inDir path $
    rawSystemStdout silent "sh" ["-c", "pwd"]
  return $ reverse . dropWhile isSpace . reverse . dropWhile isSpace $ canonicalPath


libClangConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags
                 -> IO LocalBuildInfo
libClangConfHook (pkg, pbi) flags = do
  let verbosity = fromFlag (configVerbosity flags)
      lookupConfFlag flagName defaultValue = fromMaybe defaultValue $
            lookup (FlagName flagName) (configConfigurationsFlags flags)
      debug = lookupConfFlag "debug" False
      cppinliner = lookupConfFlag "cppinliner" True

  lbi <- confHook simpleUserHooks (pkg, pbi) flags

  when cppinliner $ do
      -- Infer which standard library to use.
      cppStdLib <- preferredStdLib (withPrograms lbi) flags
      let (_, confCPPStdLib) =
            case cppStdLib of
              LibStdCPP -> ("-lstdc++", "no")
              LibCPP    -> ("-lc++", "yes")
          clangSubDir = if debug then "clangbuilddebug" else "clangbuild"

      -- Compute some paths that need to be absolute.
      curDir <- getCurrentDirectory
      let llvmRepoDir   = curDir </> "cbits" </> "llvm"
          llvmBuildDir  = curDir </> "cbits" </> clangSubDir
          llvmPrefixDir = llvmBuildDir </> "out"
          clangRepoDir  = curDir </> "cbits" </> "clang"
          clangLinkPath = llvmRepoDir </> "tools" </> "clang"

      createDirectoryIfMissingVerbose verbosity True llvmPrefixDir

      [clangRepoDirCanonical, llvmPrefixDirCanonical, llvmRepoDirCanonical] <-
        mapM canonicalizePath [clangRepoDir, llvmPrefixDir, llvmRepoDir]

      let configurePath = llvmRepoDirCanonical ++ "/configure"
          llvmArgs      =  [ "--with-clang-srcdir=" ++ clangRepoDirCanonical
                           , "--disable-polly"
                           , "--disable-shared"
                           -- https://ghc.haskell.org/trac/ghc/ticket/9657
                           , "--disable-pic"
                           , "--enable-bindings=none"
                           , "--disable-clang-arcmt"
                           , "--disable-clang-static-analyzer"
                           {-, "--disable-clang-rewriter"-}
                           , "--disable-assertions"
                           , "--disable-keep-symbols"
                           , "--disable-jit"
                           , "--disable-docs"
                           , "--disable-doxygen"
                           , "--disable-threads"
                           {-, "--disable-pthreads"-}
                           , "--disable-zlib"
                           , "--enable-targets=x86"
                           , "--disable-terminfo"
                           , "--enable-bindings=none"
                           , "--enable-libcpp=" ++ confCPPStdLib
                           , "--prefix=" ++ llvmPrefixDirCanonical
                           ] ++
                           ["CXXFLAGS=-D_GLIBCXX_HAVE_FENV_H=1 -D_WINVER=0x0502 -D_WIN32_WINNT=0x0502" | buildOS == Windows] ++


                           {-
                           -- mingw can't handle files this large
                           if debug
                             then ["--disable-optimized"]
                             else ["--enable-optimized" , "--with-optimize-option=-O2"]
                             -}
                           if debug
                              then ["--enable-debug-symbols", "--enable-debug-runtime"]
                              else []


          handleNoWindowsSH action
            | buildOS /= Windows = action
            | otherwise          = action `catchIO` \ioe -> if isDoesNotExistError ioe
                                     then die notFoundMsg
                                     else throwIO ioe
          catchIO = Control.Exception.catch
          notFoundMsg = "The package has a './configure' script. This requires a "
                     ++ "Unix compatibility toolchain such as MinGW+MSYS or Cygwin."

      -- Ensure that the LLVM build process sees clang.
      createDirectoryIfMissingVerbose verbosity True llvmPrefixDir
      clangLinkExists <- doesDirectoryExist clangLinkPath

      makefileExists <- doesFileExist $ llvmBuildDir </> "Makefile"
      unless makefileExists $ do
        notice verbosity "Configuring LLVM and Clang..."

        inDir llvmBuildDir $
          handleNoWindowsSH $
            rawSystemExit verbosity "sh" (configurePath:llvmArgs)

  return lbi

-- A version of readEntry that uses strict ByteStrings to read the file
readEntry' :: [ZipOption] -> FilePath -> IO Entry
readEntry' opts path = do
    isDir <- doesDirectoryExist path
    -- make sure directories end in / and deal with the OptLocation option
    let path' = let p = path ++ (case reverse path of
                                    ('/':_)       -> ""
                                    _ | isDir     -> "/"
                                      | otherwise -> "") in
                    (case [(l,a) | OptLocation l a <- opts] of
                        ((l,a):_) -> if a then l </> p else l
                        _ -> p)
    contents <- if isDir
                then return B.empty
                else B.fromStrict <$> BS.readFile path
    modEpochTime <- fmap (floor . utcTimeToPOSIXSeconds) $ getModificationTime path
    return $ toEntry path' modEpochTime contents

-- A version of addFilesToArchive that uses strict ByteStrings to read files
addFilesToArchive' :: [ZipOption] -> Archive -> [FilePath] -> IO Archive
addFilesToArchive' opts archive files = do
    filesAndChildren <- if OptRecursive `elem` opts
        then mapM getDirectoryContentsRecursive files >>= return . nub . concat
        else return files
    entries <- mapM (readEntry' opts) filesAndChildren
    return $ foldr addEntryToArchive archive entries


-- Zip resources. The archive will be embedded into the executable.
zipResources :: FilePath -> Verbosity -> Maybe FilePath -> IO ()
zipResources curDir verbosity llvmPrefixDir = do
    let initFile = curDir </> "res" </> "init.zip"
    zipFileExists <- doesFileExist initFile
    unless zipFileExists $ do
        notice verbosity "Zipping resource files..."

        let addFilesToZipFile :: Archive -> FilePath -> IO Archive
            addFilesToZipFile archive filesPath = inDir filesPath $
                addFilesToArchive' [OptRecursive] archive ["."]

        archive <- addFilesToZipFile emptyArchive $ curDir </> "res" </> "init"
        case llvmPrefixDir of
            Nothing -> B.writeFile initFile $ fromArchive archive
            Just dir -> do
                archive' <- addFilesToZipFile archive $ dir </> "lib" </> "clang" </> "3.6.0"
                B.writeFile initFile $ fromArchive archive'


libClangBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
libClangBuildHook pkg lbi usrHooks flags = do
  let verbosity = fromFlag (buildVerbosity flags)
      lookupConfFlag flagName defaultValue = fromMaybe defaultValue $
            lookup (FlagName flagName) (configConfigurationsFlags $ configFlags lbi)
      debug = lookupConfFlag "debug" False
      cppinliner = lookupConfFlag "cppinliner" True
  curDir <- getCurrentDirectory

  -- Build C++ library, if necessary
  if cppinliner
     then do
        -- Infer which standard library to use.
        cppStdLib <- preferredStdLib (withPrograms lbi) (configFlags lbi)
        let linkCPPStdLib = case cppStdLib of
              LibStdCPP -> "stdc++"
              LibCPP    -> "c++"
            clangSubDir = if debug then "clangbuilddebug" else "clangbuild"
            llvmBuildDir  = curDir </> "cbits" </> clangSubDir
            llvmPrefixDir = llvmBuildDir </> "out"
            llvmLibDir = llvmPrefixDir </> "lib"

            addCWrapper bi = bi {
              extraLibs = ["chelper",
                           "cpphelper",
                           "clangTooling",
                           "clangFrontendTool",
                           "clangFrontend",
                           "clangDriver",
                           "clangSerialization",
                           "clangCodeGen",
                           "clangParse",
                           "clangSema",
                           "clangAnalysis",
                           "clangRewriteFrontend",
                           "clangRewrite",
                           "clangEdit",
                           "clangAST",
                           "clangLex",
                           "clangBasic",
                           "LLVMInstrumentation",
                           "LLVMIRReader",
                           "LLVMAsmParser",
                           "LLVMDebugInfo",
                           "LLVMOption",
                           "LLVMLTO",
                           "LLVMLinker",
                           "LLVMipo",
                           "LLVMVectorize",
                           "LLVMBitWriter",
                           "LLVMBitReader",
                           "LLVMTableGen",
                           "LLVMX86Disassembler",
                           "LLVMX86AsmParser",
                           "LLVMX86CodeGen",
                           "LLVMSelectionDAG",
                           "LLVMAsmPrinter",
                           "LLVMX86Desc",
                           "LLVMX86Info",
                           "LLVMX86AsmPrinter",
                           "LLVMX86Utils",
                           "LLVMMCDisassembler",
                           "LLVMMCParser",
                           "LLVMInterpreter",
                           "LLVMMCJIT",
                           "LLVMCodeGen",
                           "LLVMObjCARCOpts",
                           "LLVMScalarOpts",
                           "LLVMInstCombine",
                           "LLVMTransformUtils",
                           "LLVMipa",
                           "LLVMAnalysis",
                           "LLVMRuntimeDyld",
                           "LLVMExecutionEngine",
                           "LLVMTarget",
                           "LLVMMC",
                           "LLVMObject",
                           "LLVMCore",
                           "LLVMSupport",
                           linkCPPStdLib] ++
                           ["imagehlp" | buildOS == Windows]
            }

            addGHCArgs = onProgram ghcProgram
                       . onProgramOverrideArgs
                       $ (++ ["-optl-Wl,-rpath," ++ libdir (absoluteInstallDirs pkg lbi NoCopyDest)])

            lbi' = onLocalLibBuildInfo addCWrapper . onPrograms addGHCArgs $ lbi


        libclangExists <- doesFileExist $ llvmLibDir </> "libclangTooling.a"
        unless libclangExists $ do
            notice verbosity "Building LLVM and Clang..."

            -- FIXME: We'd ideally like to use the -j option given to cabal-install itself.
            -- Alternatively we could use a command-specific option like
            -- 'cabal install --make-option=-j4', but see
            -- https://github.com/haskell/cabal/issues/1380 for why this doesn't work.
            -- For now we hardcore "-j4".

            -- -j4 hangs in MinGW on 64bit windows
            let threadFlags = ["-j4" | buildOS /= Windows]
            --let threadFlags = ["-j4"]
            inDir llvmBuildDir $ do
              -- Workaround for 'make not found' in MinGW
              buildMade <- case buildOS of
                  Windows   -> do
                    (exitCode, _, _) <- readProcessWithExitCode "make" threadFlags ""
                    return $ exitCode == ExitSuccess
                  otherwise -> return False

              --runDbProgram verbosity makeProgram (withPrograms lbi) threadFlags
              unless buildMade $
                rawSystemExit verbosity "make" threadFlags
              rawSystemExit verbosity "make" $ threadFlags ++ ["install"]


        notice verbosity "Building C wrapper library..."
        inDir (curDir </> "cbits") $
          runDbProgram verbosity makeProgram (withPrograms lbi') ["CAIDE_DEBUG=1" | debug]

        zipResources curDir verbosity $ Just llvmPrefixDir

        -- Build Haskell code
        buildHook simpleUserHooks (localPkgDescr lbi') lbi' usrHooks flags

      else do
        -- No cppinliner flag
        zipResources curDir verbosity Nothing
        buildHook simpleUserHooks (localPkgDescr lbi) lbi usrHooks flags


libClangCleanHook :: PackageDescription -> () -> UserHooks -> CleanFlags -> IO ()
libClangCleanHook pkg v hooks flags = do
  curDir <- getCurrentDirectory
  let verbosity = fromFlag (cleanVerbosity flags)
      buildDir = curDir </> "cbits" </> "build"
      resourcesZipFile = curDir </> "res" </> "init.zip"
  buildDirExists <- doesDirectoryExist buildDir
  when buildDirExists $ removeDirectoryRecursive buildDir
  resourcesZipFileExists <- doesFileExist resourcesZipFile
  when resourcesZipFileExists $ removeFile resourcesZipFile

  cleanHook simpleUserHooks pkg v hooks flags

  notice verbosity "LLVM and Clang were NOT cleaned! Remove clangbuild folders manually to trigger their rebuild"



mkStaticLib :: String -> String
--mkStaticLib lname = mkLibName (LibraryName lname)
mkStaticLib lname = "lib" ++ lname <.> "a"


confProgram, makeProgram, swVersProgram :: Program
confProgram    = simpleProgram "configure"
makeProgram    = simpleProgram "make"
swVersProgram  = simpleProgram "sw_vers"

inDir :: FilePath -> IO a -> IO a
inDir dir act = do
  curDir <- getCurrentDirectory
  bracket_ (setCurrentDirectory dir)
           (setCurrentDirectory curDir)
           act

data CPPStdLib = LibStdCPP
               | LibCPP
                 deriving (Eq, Show)

preferredStdLib :: ProgramDb -> ConfigFlags -> IO CPPStdLib
preferredStdLib pdb flags =
  case (preferLibStdCPP, preferLibCPP, os) of
    (Just True, _, _)     -> return LibStdCPP
    (_, Just True, _)     -> return LibCPP
    (_, _, "darwin")      -> darwinPreferredStdLib pdb flags
    _                     -> return LibStdCPP
  where
    preferLibCPP    = lookup (FlagName "preferlibcpp") $ configConfigurationsFlags flags
    preferLibStdCPP = lookup (FlagName "preferlibstdcpp") $ configConfigurationsFlags flags

darwinPreferredStdLib :: ProgramDb -> ConfigFlags -> IO CPPStdLib
darwinPreferredStdLib pdb flags = do
  let verbosity = fromFlag (configVerbosity flags)
  darwinVer <- getDbProgramOutput verbosity swVersProgram pdb ["-productVersion"]

  -- We want LibStdCPP for 10.8 and below, but LibCPP for anything newer.
  let libStdCPPVers = ["10.0", "10.1", "10.2", "10.3", "10.4", "10.5", "10.6", "10.7", "10.8"]
  return $ if any (`isPrefixOf` darwinVer) libStdCPPVers
             then LibStdCPP
             else LibCPP

type Lifter a b = (a -> a) -> b -> b

onLocalPkgDescr :: Lifter PackageDescription LocalBuildInfo
onLocalPkgDescr f lbi = lbi { localPkgDescr = f (localPkgDescr lbi) }

onPrograms :: Lifter ProgramDb LocalBuildInfo
onPrograms f lbi = lbi { withPrograms = f (withPrograms lbi) }

onExeBuildInfo :: Lifter BuildInfo Executable
onExeBuildInfo f exe = exe { buildInfo = f (buildInfo exe) }

onLocalLibBuildInfo :: Lifter BuildInfo LocalBuildInfo
onLocalLibBuildInfo = onLocalPkgDescr . onExecutables . onExeBuildInfo

onExecutables :: Lifter Executable PackageDescription
onExecutables f pd = pd { executables = map f (executables pd) }

onIncludeDirs :: Lifter [FilePath] BuildInfo
onIncludeDirs f libbi = libbi { includeDirs = f (includeDirs libbi) }

onExtraLibs :: Lifter [FilePath] BuildInfo
onExtraLibs f libbi = libbi { extraLibs = f (extraLibs libbi) }

onExtraLibDirs :: Lifter [FilePath] BuildInfo
onExtraLibDirs f libbi = libbi { extraLibDirs = f (extraLibDirs libbi) }

onProgram :: Program -> Lifter ConfiguredProgram ProgramDb
onProgram prog f pdb = case lookupProgram prog pdb of
                         Just cProg -> updateProgram (f cProg) pdb
                         Nothing    -> pdb

onProgramOverrideArgs :: Lifter [String] ConfiguredProgram
onProgramOverrideArgs f prog = prog { programOverrideArgs = f (programOverrideArgs prog) }

