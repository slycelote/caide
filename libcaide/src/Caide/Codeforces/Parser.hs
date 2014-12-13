module Caide.Codeforces.Parser(
    codeforcesParser
) where

import qualified Data.Text as T

import Network.URI (parseURI, uriAuthority, uriRegName)

import Text.HTML.TagSoup (fromTagText, innerText, isTagOpenName, isTagCloseName,
                          parseTags, sections, Tag(TagText), (~==), (~/=))

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
    Just auth -> uriRegName auth `elem` ["codeforces.com", "www.codeforces.com"]

doParseTagSoup :: URL -> IO (Either String (Problem, [TestCase]))
doParseTagSoup url = do
    doc' <- downloadDocument url
    case doc' of
        Left err -> return $ Left err
        Right cont -> do
            let tags = parseTags cont
                statement = dropWhile (~/= "<div class=problem-statement>") tags
                titleDiv = head . drop 1 . dropWhile (~/= "<div class=title>") $ statement
                title = fromTagText titleDiv
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
            return . Right $ (Problem title ("problem" ++ [T.head title]), testCases)

