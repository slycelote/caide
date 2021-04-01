{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.Common(
      replaceBr
    , mergeTextTags
    , normalizeTestCases
    , URL
    , ProblemParser(..)
    , CHelperProblemParser(..)
    , HtmlProblemParser
    , ContestParser(..)
    , makeProblemParser
    , isHostOneOf
) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (groupBy)
import qualified Data.Text as T

import Network.URI (parseURI, URI(uriAuthority), URIAuth(uriRegName))

import Text.HTML.TagSoup (Tag(..), isTagCloseName, isTagOpenName, isTagText, fromTagText)
import Text.HTML.TagSoup.Utils (isTagName)
import Text.StringLike (StringLike, strConcat)

import Network.HTTP.Util (downloadDocument)
import Caide.Types (Problem, TestCase(TestCase), CaideIO)


type URL = T.Text

data ProblemParser = ProblemParser
    { problemUrlMatches :: URL -> Bool
    , parseProblem      :: URL -> Maybe T.Text -> IO (Either T.Text (Problem, [TestCase]))
    }

data CHelperProblemParser = CHelperProblemParser
    { chelperId    :: !T.Text
    , chelperUrlMatches :: URL -> Bool
    , chelperParse :: T.Text -> IO (Either T.Text (Problem, [TestCase]))
    }

type HtmlProblemParser = T.Text -> IO (Either T.Text (Problem, [TestCase]))

data ContestParser = ContestParser
    { contestUrlMatches :: URL -> Bool
    , parseContest      :: URL -> CaideIO ()
    }

makeProblemParser :: (URL -> Bool) -> HtmlProblemParser -> ProblemParser
makeProblemParser matchPredicate htmlParser = ProblemParser matchPredicate parseImpl
  where
    parseImpl url mbHtmlNoJs = do
        htmlNoJs <- case mbHtmlNoJs of
            Just h  -> pure $ Right h
            Nothing -> downloadDocument url
        either (pure . Left) htmlParser htmlNoJs

isHostOneOf :: [String] -> URL -> Bool
isHostOneOf hosts url = (url & T.unpack & parseURI >>= uriAuthority <&> uriRegName) `elem` (map Just hosts)

-- | Replace \r\n with \n, strip all lines
normalizeText :: T.Text -> T.Text
normalizeText = T.strip . T.unlines . map T.stripEnd . T.lines

normalizeTestCases :: [TestCase] -> [TestCase]
normalizeTestCases = map (\(TestCase i o) -> TestCase (normalizeText i) (normalizeText o))

-- | Replaces <br> tags with newlines. Neighbor <br></br> pairs are replaced with a single newline.
replaceBr :: [Tag T.Text] -> [Tag T.Text]
replaceBr [] = []

replaceBr (o:c:rest)
    | isTagOpenName "br" o && isTagCloseName "br" c   = TagText "\n" : replaceBr rest

replaceBr (t:rest)
    | isTagName "br" t = TagText "\n" : replaceBr rest
    | otherwise        = t : replaceBr rest


-- | Merges adjacent text nodes into a single text node
mergeTextTags :: (StringLike str, Show str) => [Tag str] -> [Tag str]
mergeTextTags = map merge . groupBy cmp
  where
    cmp t1 t2 = isTagText t1 && isTagText t2

    merge tags@(TagText _ : _) = TagText . strConcat . map fromTagText $ tags
    merge [t] = t
    merge _ = error "mergeTextTags"

