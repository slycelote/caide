module Caide.Commands(
      runMain
) where

import Control.Monad (void)
import Data.Monoid (mconcat)
import qualified Data.Text as T
import Options.Applicative

import Caide.Types (CaideIO)
import qualified Caide.Commands.Init as Init
import Caide.Commands.ParseProblem
import Caide.Commands.ParseContest
import Caide.Commands.BuildScaffold
import Caide.Commands.Checkout
import Caide.Commands.Archive
import Caide.Commands.RunTests
import Caide.Commands.Make
import Caide.Commands.GetOpt

createSubCommand :: (String, String, Parser (CaideIO ())) -> Mod CommandFields (CaideIO ())
createSubCommand (name, desc, cmd) = command name $
    info (helper <*> cmd) $ progDesc desc <> fullDesc

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

    , ("getopt", "(Internal) print config option", optionsCmd getOpt)
    , ("getstate", "(Internal) print caide state", optionsCmd getState)
    , ("probgetopt", "(Internal) print problem config option", probOptionsCmd getProbOpt)
    , ("probgetstate", "(Internal) print problem state", probOptionsCmd getProbState)
    , ("update_tests", "(Internal) Update test list and status", pure updateTests)
    , ("eval_tests", "(Internal) Generate test report", pure evalTests)
    ]

initOpts :: Parser (CaideIO ())
initOpts = Init.initialize <$>
    switch (long "cpp-use-system-headers" <> help "Use system headers for C++ code inliner, instead of builtin MinGW headers")

problemOpts :: Parser (CaideIO ())
problemOpts = createProblem <$>
    txtArgument (metavar "URL-OR-NAME" <> help "Either problem URL to parse or problem name to create from scratch")

contestOpts :: Parser (CaideIO ())
contestOpts = createContest <$>
    txtArgument (metavar "URL" <> help "Contest URL")

langOpts :: Parser (CaideIO ())
langOpts = generateScaffoldSolution <$>
    txtArgument (metavar "LANG" <> help "Programming language of the solution (cpp, csharp)")

checkoutOpts :: Parser (CaideIO ())
checkoutOpts = checkoutProblem <$>
    txtArgument (metavar "PROBLEM" <> help "Problem ID (matches problem directory)")

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

opts :: ParserInfo (CaideIO ())
opts = info (helper <*> (subparser . mconcat . map createSubCommand $ commands)) $
    fullDesc <> header "Caide -- programming competitions tool"

runMain :: [String] -> Either (IO ()) (CaideIO ())
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

