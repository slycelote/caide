module Caide.Parsers.Codeforces (
    codeforcesParser
) where

import Data.Array ((!))
import qualified Data.Text as T

import Network.URI (parseURI, uriAuthority, uriRegName)

import Text.HTML.TagSoup (fromTagText, innerText, isTagOpenName, isTagCloseName,
                          parseTags, sections, fromAttrib, Tag(TagText), (~==), (~/=))

import Text.Regex.TDFA.Text (Regex)
import Text.Regex.Base.RegexLike (makeRegex, matchAllText)

import Caide.Types
import Caide.Util (downloadDocument)


codeforcesParser :: ProblemParser
codeforcesParser = ProblemParser
    { matches = isCodeForcesUrl
    , parse = doParseTagSoup
    }

isCodeForcesUrl :: URL -> Bool
isCodeForcesUrl url = case parseURI (T.unpack url) >>= uriAuthority of
    Nothing   -> False
    Just auth -> uriRegName auth `elem` ["codeforces.com", "www.codeforces.com", "codeforces.ru", "www.codeforces.ru"]

doParseTagSoup :: URL -> IO (Either String (Problem, [TestCase]))
doParseTagSoup url = do
    doc' <- downloadDocument url
    case doc' of
        Left err -> return $ Left err
        Right cont -> do
            let tags = parseTags cont
                statement = dropWhile (~/= "<div class=problem-statement>") tags
                beforeTitleDiv = drop 1 . dropWhile (~/= "<div class=title>") $ statement
                title = fromTagText $ head beforeTitleDiv
                inputDivs = sections (~== "<div class=input>") statement
                outputDivs = sections (~== "<div class=output>") statement
                replaceBr = concatMap f
                    where f x | isTagOpenName (T.pack "br") x = []
                              | isTagCloseName (T.pack "br") x = [TagText $ T.pack "\n"]
                              | otherwise = [x]

                extractText = innerText . replaceBr . takeWhile (~/= "</pre>") . dropWhile (~/= "<pre>")
                inputs = map extractText inputDivs
                outputs = map extractText outputDivs
                testCases = zipWith TestCase inputs outputs

                -- Contest
                sidebar = dropWhile (~/= "<div id=sidebar>") tags
                rtable = takeWhile (~/= "</tbody>") . dropWhile (~/= "<tbody>") $ sidebar
                anchors = sections (~== "<a>") rtable
                links = map (fromAttrib (T.pack "href") . head) anchors
                allMatches = concatMap (matchAllText contestUrlRegex) links
                contestIds = map (fst . (!1)) allMatches

                probIdPrefix = if length contestIds == 1
                               then "cf" ++ T.unpack (head contestIds)
                               else "cfproblem"
                probId = probIdPrefix ++ [T.head title]
            if null beforeTitleDiv
                then return . Left $ "Couldn't parse problem statement"
                else return . Right $ (Problem title probId, testCases)


contestUrlRegex :: Regex
contestUrlRegex = makeRegex ".*/([[:digit:]]+)$"

