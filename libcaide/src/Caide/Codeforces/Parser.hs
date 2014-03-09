module Caide.Codeforces.Parser(
    codeforcesParser
) where

import Caide.Types
import Caide.Util (downloadDocument)

import qualified Data.Text as T

import Text.Regex (mkRegex, subRegex)

import Text.XML.HaXml.Combinators
import Text.XML.HaXml.Html.Parse (htmlParse')
import Text.XML.HaXml
import Text.XML.HaXml.Posn (posInNewCxt)
import Text.XML.HaXml.Util (docContent, tagTextContent)

-- | Filter searching for specific tag with specific attribute having specific value
tagWithAttrValue :: String -> String -> String -> CFilter i
tagWithAttrValue tagName attrName attrValue = tag tagName `o` attrval (attName, attValue)
    where attName = N attrName
          attValue = AttValue [Left attrValue]

codeforcesParser :: ProblemParser
codeforcesParser url = do
    doc' <- downloadDocument url
    case doc' of
        Left err -> return $ Left err
        Right cont -> do
            let dummyFileName = "problem.html"
                parseResult = htmlParse' dummyFileName (stripUnicodeBOM $ T.unpack cont)
                
                Right doc = parseResult
                rootElem = docContent (posInNewCxt dummyFileName Nothing) doc
                
                problemStatementFilter = deep $ tagWithAttrValue "div" "class" "problem-statement"
                titleFilter = problemStatementFilter /> 
                              tagWithAttrValue "div" "class" "header" /> 
                              tagWithAttrValue "div" "class" "title"


                titles = map tagTextContent $ titleFilter rootElem

                inputFilter  = deep (tagWithAttrValue "div" "class" "sample-test") />
                               tagWithAttrValue "div" "class" "input" />
                               tag "pre" 
                outputFilter = deep (tagWithAttrValue "div" "class" "sample-test") />
                               tagWithAttrValue "div" "class" "output" />
                               tag "pre"

                postprocess s = subRegex (mkRegex "<br\\s*/?>") s "\r\n"
                inputs  = map (postprocess . tagTextContent) $ inputFilter rootElem
                outputs = map (postprocess . tagTextContent) $ outputFilter rootElem                               

                testCases = zipWith TestCase inputs outputs                  

                {- | Some Unicode documents begin with a binary sequence;
                   strip it off before processing. -}
                stripUnicodeBOM :: String -> String
                stripUnicodeBOM ('\xef':'\xbb':'\xbf':s) = s
                stripUnicodeBOM s = s
               
            case parseResult of
                Left err -> return . Left $ err
                _ ->  case titles of
                    [title] -> return . Right $ (Problem (T.pack title) undefined, testCases)
                    []      -> return . Left $ "No title found"
                    _       -> return . Left $ "More than one title found"

