module Caide.Parsers.CodeChef (
    codeChefParser
) where

import qualified Data.Text as T

import Network.URI (parseURI, uriAuthority, uriRegName)

import Text.HTML.TagSoup (Tag, innerText, parseTags, sections, (~==), (~/=))

import Caide.Types
import Caide.Util (downloadDocument)


codeChefParser :: ProblemParser
codeChefParser = ProblemParser
    { problemUrlMatches = isCodeChefUrl
    , parseProblem = doParse
    }

isCodeChefUrl :: URL -> Bool
isCodeChefUrl url = case parseURI (T.unpack url) >>= uriAuthority of
    Nothing   -> False
    Just auth -> uriRegName auth `elem` ["codechef.com", "www.codechef.com"]

doParse :: URL -> IO (Either String (Problem, [TestCase]))
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
                probId = "chef" ++ T.unpack title

                -- test cases
                problemPage = dropWhile (~/= "<div id=problem-page>") tags
                content = takeWhile (~/= "</div>") . dropWhile (~/= "<div class=content>") $ problemPage
                samples = sections (~== "<pre>") content
                parseSample pre = TestCase (T.strip $ innerText input) (T.strip $ innerText output)
                    where input = takeWhile (~/= "<b>") . skipTag "b" $ pre
                          output = takeWhile (~/= "</pre>") . skipTag "b" . skipTag "b" $ pre
                testCases = map parseSample samples

            if null problemCode
                then return . Left $ "Couldn't parse problem statement"
                else return . Right $ (Problem title probId, testCases)

skipTag :: String -> [Tag T.Text] -> [Tag T.Text]
skipTag tagName = drop 1 . dropWhile (~/= ("</" ++ tagName ++ ">")) . dropWhile (~/= ("<" ++ tagName ++ ">"))

