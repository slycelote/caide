{-# LANGUAGE TupleSections #-}

import Codec.Archive.Zip (fromArchive, addFilesToArchive, emptyArchive, findEntryByPath,
                          filesInArchive, addEntryToArchive,
                          Archive(), Entry(eRelativePath, Entry), ZipOption(..))
import Control.Exception
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Lazy as B
import Data.Char(isSpace)
import Data.List (isPrefixOf, intersperse)
import Data.Maybe (fromMaybe)
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


libclangSharedLibraries :: [String]
libclangSharedLibraries = ["clang", "LLVM-3.4"]


getTemplateFiles :: FilePath -> IO [(String, String)]
getTemplateFiles dir = do
    files <- getDirectoryContents dir
    forM [f | f <- files, head f /= '.'] $ \fileName -> do
        contents <- readFile $ dir </> fileName
        return (fileName, contents)

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
                           , "--enable-bindings=none"
                           {-, "--disable-clang-arcmt"-}
                           {-, "--disable-clang-static-analyzer"-}
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
--                           , "CXXFLAGS=-D_GLIBCXX_HAVE_FENV_H=1"
                           ] ++
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


libClangBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
libClangBuildHook pkg lbi usrHooks flags = do
  let verbosity = fromFlag (buildVerbosity flags)
      lookupConfFlag flagName defaultValue = fromMaybe defaultValue $
            lookup (FlagName flagName) (configConfigurationsFlags $ configFlags lbi)
      debug = lookupConfFlag "debug" False
      cppinliner = lookupConfFlag "cppinliner" True
  curDir <- getCurrentDirectory

  -- Build list of file templates
  defaultTemplates <- getTemplateFiles (curDir </> "templates")
  let defaultTemplatesInc = curDir </> "src" </> "Caide" </> "Commands" </> "defaultTemplates.inc"
  writeFile defaultTemplatesInc $ unlines $ map ("  " ++) . intersperse ", " $ map show defaultTemplates

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
              extraLibs = ["chelper", "cpphelper", "clang", "clangFrontendTool", "clangFrontend", "clangDriver", "clangSerialization", "clangCodeGen", "clangParse", "clangSema", "clangStaticAnalyzerFrontend", "clangStaticAnalyzerCheckers", "clangStaticAnalyzerCore", "clangAnalysis", "clangARCMigrate", "clangRewriteFrontend", "clangRewriteCore", "clangEdit", "clangAST", "clangLex", "clangBasic", "LLVMInstrumentation", "LLVMIRReader", "LLVMAsmParser", "LLVMDebugInfo", "LLVMOption", "LLVMLTO", "LLVMLinker", "LLVMipo", "LLVMVectorize", "LLVMBitWriter", "LLVMBitReader", "LLVMTableGen", "LLVMX86Disassembler", "LLVMX86AsmParser", "LLVMX86CodeGen", "LLVMSelectionDAG", "LLVMAsmPrinter", "LLVMX86Desc", "LLVMX86Info", "LLVMX86AsmPrinter", "LLVMX86Utils", "LLVMMCDisassembler", "LLVMMCParser", "LLVMInterpreter", "LLVMMCJIT", "LLVMJIT", "LLVMCodeGen", "LLVMObjCARCOpts", "LLVMScalarOpts", "LLVMInstCombine", "LLVMTransformUtils", "LLVMipa", "LLVMAnalysis", "LLVMRuntimeDyld", "LLVMExecutionEngine", "LLVMTarget", "LLVMMC", "LLVMObject", "LLVMCore", "LLVMSupport", linkCPPStdLib] ++
                            ["imagehlp" | buildOS == Windows]
            }

            addGHCArgs = onProgram ghcProgram
                       . onProgramOverrideArgs
                       $ (++ (["-optl-static" | buildOS == Windows]
                              ++ ["-optl-Wl,-rpath," ++ libdir (absoluteInstallDirs pkg lbi NoCopyDest)])
                        )

            lbi' = onLocalLibBuildInfo addCWrapper . onPrograms addGHCArgs $ lbi


        libclangExists <- doesFileExist $ llvmLibDir </> "libclang.a"
        unless libclangExists $ do
            notice verbosity "Building LLVM and Clang..."

            -- FIXME: We'd ideally like to use the -j option given to cabal-install itself.
            -- Alternatively we could use a command-specific option like
            -- 'cabal install --make-option=-j4', but see
            -- https://github.com/haskell/cabal/issues/1380 for why this doesn't work.
            -- For now we hardcore "-j4".
            inDir llvmBuildDir $ do
              -- Temporary HACK for MinGW
              buildMade <- case buildOS of
                  Windows   -> do
                    (exitCode, _, _) <- readProcessWithExitCode "make" [] ""
                    return $ exitCode == ExitSuccess
                  otherwise -> return False

              --runDbProgram verbosity makeProgram (withPrograms lbi) ["-j4" | buildOS /= Windows]
              -- -j4 hangs in MinGW on 64bit windows
              unless buildMade $
                rawSystemExit verbosity "make"   ["-j4" | buildOS /= Windows]
              rawSystemExit verbosity "make" $ ["-j4" | buildOS /= Windows] ++ ["install"]

            -- OS X's linker _really_ wants to link dynamically, and it doesn't support
            -- the options you'd usually use to control that on Linux. We rename the
            -- libclang library to make sure the linker does what we intend.
            copyFileVerbose verbosity (llvmLibDir </> mkStaticLib "clang")
                                      (llvmLibDir </> mkStaticLib "clang_static")


        notice verbosity "Building C wrapper library..."
        inDir (curDir </> "cbits") $
          runDbProgram verbosity makeProgram (withPrograms lbi') ["CAIDE_DEBUG=1" | debug]


        -- Zip headers. The archive will be embedded into the executable.
        notice verbosity "Zipping header files..."

        let addFilesToZipFile :: Archive -> FilePath -> IO Archive
            addFilesToZipFile archive filesPath = inDir filesPath $
                addFilesToArchive [OptRecursive] archive ["."]

        archive <- addFilesToZipFile emptyArchive $
            llvmPrefixDir </> "lib" </> "clang" </> "3.4.2"
        archive' <- addFilesToZipFile archive $ curDir </> "res" </> "include"
        B.writeFile (curDir </> "res" </> "headers.zip") $ fromArchive archive'

        -- Build Haskell code
        buildHook simpleUserHooks (localPkgDescr lbi') lbi' usrHooks flags

      else
        -- No cppinliner flag
        buildHook simpleUserHooks (localPkgDescr lbi) lbi usrHooks flags


  notice verbosity "Relinking..."


libClangCopyHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
libClangCopyHook pkg lbi hooks flags = do
  copyHook simpleUserHooks pkg lbi hooks flags

  curDir <- getCurrentDirectory
  let verbosity = fromFlag (copyVerbosity flags)
      lookupConfFlag flagName defaultValue = fromMaybe defaultValue $
            lookup (FlagName flagName) (configConfigurationsFlags $ configFlags lbi)
      debug = lookupConfFlag "debug" False
      llvmLibDir = curDir </> "cbits" </> (if debug then "clangbuilddebug" else "clangbuild") </> "out" </> "lib"
      libCopyDir = libdir $ absoluteInstallDirs pkg lbi NoCopyDest

  notice verbosity "Installing libclang shared libraries..."
  copyFiles verbosity libCopyDir $ map ((llvmLibDir,) . mkSharedLib) libclangSharedLibraries


libClangCleanHook :: PackageDescription -> () -> UserHooks -> CleanFlags -> IO ()
libClangCleanHook pkg v hooks flags = do
  curDir <- getCurrentDirectory
  let verbosity = fromFlag (cleanVerbosity flags)
      buildDir = curDir </> "cbits" </> "build"
  buildDirExists <- doesDirectoryExist buildDir
  when buildDirExists $ removeDirectoryRecursive buildDir

  cleanHook simpleUserHooks pkg v hooks flags

  notice verbosity "LLVM and Clang are NOT cleaned! Remove clangbuild folders manually to trigger their rebuild"



mkStaticLib :: String -> String
--mkStaticLib lname = mkLibName (LibraryName lname)
mkStaticLib lname = "lib" ++ lname <.> "a"

mkSharedLib :: String -> String
mkSharedLib lname = "lib" ++ lname <.> dllExtension


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

