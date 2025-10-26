{-# LANGUAGE OverloadedStrings, NamedFieldPuns, RecordWildCards #-}
module Caide.Commands(
      runMain
) where

import Control.Exception.Base (catch, SomeException)
import Control.Monad.Extended (catchError, void)
import qualified Data.Text as T
import Data.Version (showVersion)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.Environment (getExecutablePath)

import qualified Filesystem.Path.CurrentOS as FS
import Filesystem.Path.CurrentOS ((</>))
import Options.Applicative
import Options.Applicative.Types (Backtracking(..))

import System.IO.Util (writeFileAtomic)

import Caide.CheckUpdates (checkUpdates, checkUpdatesCommand, logIfUpdateAvailable)
import Caide.Monad (CaideIO, RunSettings(..), run, describeError)
import qualified Caide.Commands.Init as Init
import Caide.Commands.Archive
import Caide.Commands.BuildScaffold
import Caide.Commands.Checkout
import Caide.Commands.CHelperHttpServer
import Caide.Commands.ConvertTestCase
import Caide.Commands.GetOpt
import Caide.Commands.Make
import Caide.Commands.ParseProblem (createProblem)
import Caide.Commands.ParseContest
import Caide.Commands.RunTests
import Caide.Logger (ColorOutput(..), Verbosity(Info, Debug), logError)
import Caide.Util (newDefaultHttpClient)
import Paths_libcaide (version)


data GlobalOptions = GlobalOptions
                 { optVerbosity :: Verbosity
                 , optColor :: ColorOutput
                 }

toVerbosity :: Int -> Verbosity
toVerbosity level = case level of
    0 -> Info
    _ -> Debug

parseColor :: ReadM ColorOutput
parseColor = do
    s <- str :: ReadM String
    case s of
        "auto"  -> pure ColorAuto
        "true"  -> pure ColorYes
        "false" -> pure ColorNo
        _       -> readerError "--color option requires the value of auto, true or false"

globalOptionsParser :: Parser GlobalOptions
globalOptionsParser = GlobalOptions . toVerbosity . length
    <$> many (flag' () (short 'v' <> help "Verbose output"))
    <*> option parseColor (long "color" <> short 'c' <> value ColorAuto <> metavar "true,false" <> help "Colorized output")

type CaideAction = GlobalOptions -> FS.FilePath -> IO ()

createIoSubCommand :: (String, String, Parser CaideAction) -> Mod CommandFields CaideAction
createIoSubCommand (name, desc, cmd) = command name $
    info (helper <*> cmd) $ progDesc desc <> fullDesc

data CommandExtension = AutoCheckUpdates | ReportNewVersion

extendCommand :: CaideIO () -> CommandExtension -> CaideIO ()
-- TODO: run checkUpdates in parallel with cmd
extendCommand cmd AutoCheckUpdates = cmd >> (checkUpdates `catchError` const (pure ()))
extendCommand cmd ReportNewVersion = cmd >> (logIfUpdateAvailable `catchError` const (pure ()))

caideIoToIo :: [CommandExtension] -> CaideIO () -> CaideAction
caideIoToIo extensions cmd globalOptions root = do
    httpClient <- newDefaultHttpClient
    let settings = RunSettings{
        root, httpClient
        , verbosity=optVerbosity globalOptions
        , color=optColor globalOptions
    }
    ret <- run settings $ foldl' extendCommand cmd extensions

    -- Save path to caide executable
    let fileNameStr = FS.encodeString (root </> ".caide" </> "caideExe.txt")
        ignoreException :: SomeException -> IO ()
        ignoreException = const $ return ()
    (getExecutablePath >>= \caideExe -> writeFileAtomic fileNameStr caideExe) `catch` ignoreException

    case ret of
        Left err -> do
            logError $ describeError err
            exitWith $ ExitFailure 0xCA1DE
        _        -> return ()


createSubCommand :: (String, String, [CommandExtension], Parser (CaideIO ())) -> Mod CommandFields CaideAction
createSubCommand (name, desc, extensions, cmd) = createIoSubCommand (name, desc, caideIoToIo extensions <$> cmd)

createSubCommand' :: (String, String, Parser (CaideIO ())) -> Mod CommandFields CaideAction
createSubCommand' (name, desc, cmd) = createSubCommand (name, desc, [], cmd)

-- NOTE: Not using AutoCheckUpdates everywhere because it could cause slowdown
-- when we're offline / GitHub inaccessible / etc.
commands :: [(String, String, [CommandExtension], Parser (CaideIO ()))]
commands =
    [ ("problem", "Parse a problem or create an empty problem", [AutoCheckUpdates, ReportNewVersion], problemOpts)
    , ("contest", "Parse an online contest", [AutoCheckUpdates, ReportNewVersion], contestOpts)
    , ("make", "Prepare submission file and update test list", [ReportNewVersion], makeOpts)
    , ("test", "Run tests and generate test report", [ReportNewVersion], pure runTests)
    , ("checkout", "Switch to a different problem", [ReportNewVersion], checkoutOpts)
    , ("lang", "Generate scaffold solution", [ReportNewVersion], langOpts)
    , ("archive", "Move problem to the archive", [ReportNewVersion], archiveOpts)
    , ("checkUpdates", "Check for updates", [], pure checkUpdatesCommand)
    ]

internalCommands :: [(String, String, Parser (CaideIO ()))]
internalCommands =
    [ ("getopt", "(Internal) print config option", optionsCmd getOpt)
    , ("getstate", "(Internal) print caide state", optionsCmd getState)
    , ("probgetopt", "(Internal) print problem config option", probOptionsCmd getProbOpt)
    , ("probgetstate", "(Internal) print problem state", probOptionsCmd getProbState)
    , ("update_tests", "(Internal) Update test list and status", updateTestsOpts)
    , ("eval_tests", "(Internal) Generate test report", pure evalTests)
    , ("convert_test_input", "(Internal) Convert test input from a structured (Topcoder/LeetCode) to a simple line-based format",
          convertTestCaseInputOpts)
    , ("convert_test_output", "(Internal) Convert test output from a structured (Topcoder/LeetCode) to a simple line-based format",
          convertTestCaseOutputOpts)
    , ("printRoot", "(Internal) Show caide root directory", pure printRoot)
    ]


publicSubCommands :: [Mod CommandFields CaideAction]
publicSubCommands = [
    createIoSubCommand ("init", "Initialize caide directory", initOpts) ] ++
    map createSubCommand commands ++
    [ createIoSubCommand (
        "httpServer",
        "Run HTTP server for Competitive Companion and CHelper browser extensions",
        pure $ \globalOpts -> runHttpServer (optVerbosity globalOpts) (optColor globalOpts))
    ]

internalSubCommands :: [Mod CommandFields CaideAction]
internalSubCommands = map createSubCommand' internalCommands


initOpts :: Parser CaideAction
initOpts = (\useSystemHeaders _globalOptions root -> Init.initialize root useSystemHeaders) <$>
    switch (long "cpp-use-system-headers" <>
        help "Use system headers for C++ code inliner, instead of builtin MinGW headers")


problemOpts :: Parser (CaideIO ())
problemOpts = createProblem
    <$> txtArgument (metavar "URL-OR-NAME" <>
        help "Either problem URL to parse or problem name to create from scratch")
    <*> txtOption (long "type" <> short 't' <> metavar "PROBLEM-TYPE" <>
        help "Problem type, e.g.: 'topcoder,ClassName,methodName:returnType,param1:type1,param2:type2', 'file,stdin,stdout' or 'file,input.txt,output.txt'. Topcoder types are: int, long, double, String, vint, vlong, vdouble, vString. This option is ignored when parsing an existing problem." <>
        value "file,stdin,stdout" <> showDefault)
    <*> optional (txtOption (long "lang" <> short 'l' <> metavar "LANG" <>
        help "Programming language to switch to"))
    <*> optional (txtOption (long "from-file" <> short 'f' <> metavar "PATH-TO-FILE" <>
        help "Instead of downloading web page containing the description of the problem, read it from file"))
    <*> optional (txtOption (long "id" <> metavar "ID" <>
        help "Override problem ID / directory"))


contestOpts :: Parser (CaideIO ())
contestOpts = createContest <$>
    txtArgument (metavar "URL" <> help "Contest URL")

makeOpts :: Parser (CaideIO ())
makeOpts = make <$>
    optional (txtArgument (metavar "PROBLEM" <> help "Problem ID (matches problem directory)"))

langOpts :: Parser (CaideIO ())
langOpts = generateScaffoldSolution <$>
    txtArgument (metavar "LANG" <> help "Programming language of the solution (cpp, csharp)")

checkoutOpts :: Parser (CaideIO ())
checkoutOpts = checkoutProblem <$>
    txtArgument (metavar "PROBLEM" <> help "Problem ID (matches problem directory)") <*>
    optional (txtOption (long "lang" <> short 'l' <> metavar "LANG" <> help "Programming language to switch to"))

archiveOpts :: Parser (CaideIO ())
archiveOpts = archiveProblem <$>
    txtArgument (metavar "PROBLEM" <> help "Problem ID (matches problem directory)")

optionsCmd :: (T.Text -> T.Text -> CaideIO ()) -> Parser (CaideIO ())
optionsCmd handler = handler <$>
    txtArgument (metavar "SECTION" <> help "Section name") <*>
    txtArgument (metavar "KEY" <> help "Key name")

probOptionsCmd :: (T.Text -> T.Text -> T.Text -> CaideIO ()) -> Parser (CaideIO ())
probOptionsCmd handler = handler <$>
    txtArgument (metavar "PROBLEM" <> help "Problem name") <*>
    txtArgument (metavar "SECTION" <> help "Section name") <*>
    txtArgument (metavar "KEY" <> help "Key name")

updateTestsOpts :: Parser (CaideIO ())
updateTestsOpts = updateTests <$>
    optional (txtArgument (metavar "PROBLEM" <> help "Problem ID (matches problem directory)"))

convertTestCaseInputOpts :: Parser (CaideIO ())
convertTestCaseInputOpts = convertTestCaseInput <$>
    fileArgument (metavar "TEST-INPUT-FILE" <> help "Path to the file containing test case input") <*>
    fileArgument (metavar "TEST-OUTPUT-FILE" <> help "Path to the generated file containing test case input in a simplified format") <*>
    optional (txtArgument (metavar "PROBLEM" <> help "Problem ID (matches problem directory)"))

convertTestCaseOutputOpts :: Parser (CaideIO ())
convertTestCaseOutputOpts = convertTestCaseOutput <$>
    fileArgument (metavar "TEST-INPUT-FILE" <> help "Path to the file containing test case output") <*>
    fileArgument (metavar "TEST-OUTPUT-FILE" <> help "Path to the generated file containing test case output in a simplified format") <*>
    optional (txtArgument (metavar "PROBLEM" <> help "Problem ID (matches problem directory)"))


data Options = Options
             { globalOptions :: GlobalOptions
             , caideAction :: CaideAction
             }

allCommands :: Parser CaideAction
allCommands = subparser (mconcat publicSubCommands) <|>
    subparser (mconcat internalSubCommands <> commandGroup "Internal commands:" <> internal)

optionsParser :: Parser Options
optionsParser = Options <$> globalOptionsParser <*> allCommands

opts :: ParserInfo Options
opts = info (helper <*> optionsParser) $
    fullDesc <> header ("Caide " <> showVersion version <> " -- programming competitions tool") <>
    progDesc "Additional help is available with 'caide -h' or 'caide COMMAND -h'" <>
    footer "http://github.com/slycelote/caide"

runMain :: [String] -> Either (IO ()) (FS.FilePath -> IO ())
runMain args = case parseResult of
    -- Run the parsed action
    Success (Options{..}) -> Right $ \root -> caideAction globalOptions root
    -- Handle shell completion or error and then exit the process
    _ -> Left . void $ handleParseResult parseResult
  where
    parseResult = execParserPure parsePrefs opts args
    parsePrefs = defaultPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefBacktrack = Backtrack
        , prefColumns = 80
        }

txtArgument :: Mod ArgumentFields String -> Parser T.Text
txtArgument mods = T.pack <$> strArgument mods

txtOption :: Mod OptionFields String -> Parser T.Text
txtOption mods = T.pack <$> strOption mods

fileArgument :: Mod ArgumentFields String -> Parser FS.FilePath
fileArgument mods = FS.decodeString <$> strArgument mods

