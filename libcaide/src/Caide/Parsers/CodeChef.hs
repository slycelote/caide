{-# LANGUAGE OverloadedStrings #-}
module Caide.Parsers.CodeChef (
    codeChefParser
) where

import qualified Data.Text as T

import Network.URI (parseURI, uriAuthority, uriRegName)

import Text.HTML.TagSoup (Tag(..), innerText, fromTagText, parseTags,
                         isTagCloseName, isTagOpenName)

import Caide.Types
import Caide.Util (downloadDocument)
import Text.HTML.TagSoup.Utils ((~/=), (~~/==), isTagName, mergeTextTags)


codeChefParser :: ProblemParser
codeChefParser = ProblemParser
    { problemUrlMatches = isCodeChefUrl
    , parseProblem = doParse
    }

isCodeChefUrl :: URL -> Bool
isCodeChefUrl url = case parseURI (T.unpack url) >>= uriAuthority of
    Nothing   -> False
    Just auth -> uriRegName auth `elem` ["codechef.com", "www.codechef.com"]

doParse :: URL -> IO (Either T.Text (Problem, [TestCase]))
doParse url = do
    doc' <- downloadDocument url
    case doc' of
        Left err -> return $ Left err
        Right cont -> do
            let tags = parseTags cont

                -- problem ID
                problemCode = dropWhile (~/= "<span id=problem-code>") tags
                spanContents = takeWhile (~/= "</span>") problemCode
                -- TODO title
                title = T.strip $ innerText spanContents
                probId = T.append "chef" title

                -- test cases
                problemPage = dropWhile (~/= "<div id=problem-page>") tags
                content = dropWhile (~~/== "<div class=content>") problemPage
                testsContainer = drop 1 . takeWhile (~/= "</pre>") . dropWhile (~/= "<pre>") $ content
                rootTextNodes = extractCurrentLevelTextNodes . mergeTextTags . replaceBr $ testsContainer
                inputsAndOutputs = filter (not . T.null) . map (T.strip . fromTagText) $ rootTextNodes
                testCases = [TestCase (inputsAndOutputs!!i) (inputsAndOutputs!!(i+1)) |
                                i <- [0, 2 .. length inputsAndOutputs-2]]

            if null problemCode
                then return . Left $ "Couldn't parse problem statement"
                else return . Right $ (Problem title probId, testCases)

extractCurrentLevelTextNodes :: Eq a => [Tag a] -> [Tag a]
extractCurrentLevelTextNodes tags = go tags []
  where
    go [] nodes = reverse nodes
    go (TagText s:rest) nodes = go rest (TagText s:nodes)
    go (TagOpen name _ : rest) nodes = go (dropWhile (not . isTagCloseName name) rest) nodes
    go (_:rest) nodes = go rest nodes

replaceBr :: [Tag T.Text] -> [Tag T.Text]
replaceBr [] = []

replaceBr (o:c:rest)
    | isTagOpenName "br" o && isTagCloseName "br" c   = TagText "\n" : replaceBr rest

replaceBr (t:rest)
    | isTagName "br" t = TagText "\n" : replaceBr rest
    | otherwise        = t : replaceBr rest


