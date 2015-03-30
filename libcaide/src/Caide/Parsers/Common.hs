{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.Common(
      replaceBr
    , mergeTextTags
    , normalizeText
    , htmlParserToProblemParser
) where

import Data.List (groupBy)
import qualified Data.Text as T

import Text.HTML.TagSoup (Tag(..), isTagCloseName, isTagOpenName, isTagText, fromTagText)
import Text.HTML.TagSoup.Utils (isTagName)
import Text.StringLike (StringLike, strConcat)

import Caide.Types
import Caide.Util (runHtmlParser)


-- | Replace \r\n with \n, strip
normalizeText :: T.Text -> T.Text
normalizeText = T.replace "\r\n" "\n" . T.strip

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

htmlParserToProblemParser :: HtmlParser -> ProblemParser
htmlParserToProblemParser htmlParser = ProblemParser
    { problemUrlMatches = htmlParserUrlMatches htmlParser
    , parseProblem = runHtmlParser (parseFromHtml htmlParser)
    }

