module Caide.Commands(
      runMain
) where

import Control.Monad (void)
import Data.Monoid (mconcat)
import qualified Data.Text as T
import qualified Filesystem.Path as F
import Options.Applicative
import System.Exit (exitWith, ExitCode(ExitFailure))

import Caide.Types (CaideIO, runInDirectory)
import qualified Caide.Commands.Init as Init
import Caide.Configuration (describeError)
import Caide.Commands.Archive
import Caide.Commands.BuildScaffold
import Caide.Commands.Checkout
import Caide.Commands.CHelperHttpServer
import Caide.Commands.GetOpt
import Caide.Commands.Make
import Caide.Commands.ParseProblem (createProblem)
import Caide.Commands.ParseContest
import Caide.Commands.RunTests


type CaideAction = F.FilePath -> IO ()

caideIoToIo :: CaideIO () -> CaideAction
caideIoToIo cmd root = do
    ret <- runInDirectory root cmd
    case ret of
        Left err -> do
            putStrLn $ describeError err
            exitWith $ ExitFailure 0xCA1DE
        _        -> return ()


createIoSubCommand :: (String, String, Parser CaideAction) -> Mod CommandFields CaideAction
createIoSubCommand (name, desc, cmd) = command name $
    info (helper <*> cmd) $ progDesc desc <> fullDesc

createSubCommand :: (String, String, Parser (CaideIO ())) -> Mod CommandFields CaideAction
createSubCommand (name, desc, cmd) = createIoSubCommand (name, desc, caideIoToIo <$> cmd)


commands :: [(String, String, Parser (CaideIO ()))]
commands =
    [ ("init", "Initialize caide directory", initOpts)
    , ("problem", "Parse a problem or create an empty problem", problemOpts)
    , ("contest", "Parse an online contest", contestOpts)
    , ("make", "Prepare submission file and update test list", pure make)
    , ("test", "Run tests and generate test report", pure runTests)
    , ("checkout", "Switch to a different problem", checkoutOpts)
    , ("lang", "Generate scaffold solution", langOpts)
    , ("archive", "Move problem to the archive", archiveOpts)
    ]

internalCommands :: [(String, String, Parser (CaideIO ()))]
internalCommands =
    [ ("getopt", "(Internal) print config option", optionsCmd getOpt)
    , ("getstate", "(Internal) print caide state", optionsCmd getState)
    , ("probgetopt", "(Internal) print problem config option", probOptionsCmd getProbOpt)
    , ("probgetstate", "(Internal) print problem state", probOptionsCmd getProbState)
    , ("update_tests", "(Internal) Update test list and status", pure updateTests)
    , ("eval_tests", "(Internal) Generate test report", pure evalTests)
    , ("printRoot", "(Internal) Show caide root directory", pure printRoot)
    ]


allSubCommands :: [Mod CommandFields CaideAction]
allSubCommands = map createSubCommand commands ++
    [ createIoSubCommand (
        "httpServer",
        "Run HTTP server for CHelper browser extension",
        pure runHttpServer)
    ] ++
    map createSubCommand internalCommands


initOpts :: Parser (CaideIO ())
initOpts = Init.initialize <$>
    switch (long "cpp-use-system-headers" <>
        help "Use system headers for C++ code inliner, instead of builtin MinGW headers")


problemOpts :: Parser (CaideIO ())
problemOpts = createProblem
    <$> txtArgument (metavar "URL-OR-NAME" <>
        help "Either problem URL to parse or problem name to create from scratch")
    <*> txtOption (long "type" <> short 't' <> metavar "PROBLEM-TYPE" <>
        help "Problem type, e.g.: 'topcoder', 'file,stdin,stdout' or 'file,input.txt,output.txt'. Ignored when parsing an existing problem." <>
        value "file,stdin,stdout" <> showDefault)
    <*> optional (txtOption (long "lang" <> short 'l' <> metavar "LANG" <>
        help "Programming language to switch to"))


contestOpts :: Parser (CaideIO ())
contestOpts = createContest <$>
    txtArgument (metavar "URL" <> help "Contest URL")

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

opts :: ParserInfo CaideAction
opts = info (helper <*> subparser (mconcat allSubCommands)) $
    fullDesc <> header "Caide -- programming competitions tool" <>
    progDesc "Additional help is available with 'caide -h' or 'caide COMMAND -h'" <>
    footer "http://github.com/slycelote/caide"

runMain :: [String] -> Either (IO ()) (F.FilePath -> IO ())
runMain args = case parseResult of
    Success cmd -> Right cmd
    _           -> Left . void $ handleParseResult parseResult
  where
    parseResult = execParserPure parsePrefs opts args
    parsePrefs = ParserPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefBacktrack = True
        , prefColumns = 80
        }

txtArgument :: Mod ArgumentFields String -> Parser T.Text
txtArgument mods = T.pack <$> strArgument mods

txtOption :: Mod OptionFields String -> Parser T.Text
txtOption mods = T.pack <$> strOption mods

