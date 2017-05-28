{-# LANGUAGE OverloadedStrings #-}
module Caide.DCJ (
      makeDcjProblemHeaders
) where

import Control.Monad (when)
import Control.Monad.Identity (Identity)
import Data.Either (rights)
import Data.List (partition)
import Data.Text (Text)
import qualified Data.Text as T

import Text.Parsec
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P


data Variable
    = Variable
    { varType :: !Text
    , varName :: !Text
    } deriving (Show)

data FunctionSignature
    = FunctionSignature
    { funcParams :: ![Variable]
    , funcReturnValue :: !Variable
    } deriving (Show)


data ParsedHeader
    = ParsedHeader
    { headerIncludes :: ![Text]
    , headerImpl :: ![Text]
    , headerId :: !Int
    }

makeDcjProblemHeaders :: [Text] -> (Text, Text)
makeDcjProblemHeaders [] = ("", "")
makeDcjProblemHeaders headers = (testHeader, problemApiHeader)
  where
    testHeader = T.unlines $
        includes ++ ["namespace dcj {", testInterface] ++ namespaces
        ++ [customTestCase, testCaseArray, testCaseDispatch, "} // namespace dcj"]
        ++ ["extern \"C\" {"]
        ++ map dispatchFunction functions
        ++ ["}"]
    problemApiHeader = T.unlines $
        [ "#pragma once"
        , "extern \"C\" {"
        ]
        ++ [T.concat ["    ", displayFunc f, ";"] | f <- functions]
        ++ ["}"]

    includes = concatMap headerIncludes parsedHeaders ++ ["#include <stdexcept>"]
    namespaces = map (encloseInNamespace functions) parsedHeaders
    functions = parseSignatures (head parsedHeaders)
    testInterface = makeTestInterface functions
    customTestCase = makeCustomTestCase functions
    testCaseArray = makeTestCaseArray parsedHeaders
    testCaseDispatch = "int testCaseNumber = -1;\nITestCase& getTestCase();"
    parsedHeaders = map parseHeader . zip [1..] $ headers


makeTestCaseArray :: [ParsedHeader] -> Text
makeTestCaseArray parsedHeaders = T.concat $
    [ "ITestCase* testCases[] = {&customTestCase, "
    , T.intercalate ", " [ T.concat ["&case", T.pack (show $ headerId h), "::testCase"] | h <- parsedHeaders]
    , "};"
    ]

dispatchFunction :: FunctionSignature -> Text
dispatchFunction f = T.unlines $
    [ T.concat [displayFunc f, " {"]
    , T.concat ["return ::dcj::getTestCase().", makeFuncCall f]
    , "}"
    ]

parseHeader :: (Int, Text) -> ParsedHeader
parseHeader (hid, header) = ParsedHeader { headerIncludes = includes, headerImpl = impl, headerId = hid }
  where
    (includes, impl) = partition ("#" `T.isPrefixOf`) (T.lines header)

parseSignatures :: ParsedHeader -> [FunctionSignature]
parseSignatures (ParsedHeader _ impl _ ) = rights (map (parse funcParser "") impl)


encloseInNamespace :: [FunctionSignature] -> ParsedHeader -> Text
encloseInNamespace functions (ParsedHeader _ impl hid) = T.unlines $
    [ T.concat ["namespace ", namespace, " {"] ]
    ++ impl
    ++
    [ makeTestCaseImpl functions namespace
    , "}"
    ]
  where
    namespace = T.concat ["case", T.pack (show hid)]


makeCustomTestCase:: [FunctionSignature] -> Text
makeCustomTestCase functions = T.unlines $
    [ "struct CustomTestCase: ITestCase {" ] ++
    map makeThrowingImpl functions ++
    [ "} customTestCase;" ]

makeThrowingImpl :: FunctionSignature -> Text
makeThrowingImpl f = T.unlines $
    [ T.concat [ displayFunc f, " {" ]
    , "throw std::runtime_error(\"Not implemented\");"
    , "}"
    ]

makeTestCaseImpl :: [FunctionSignature] -> Text -> Text
makeTestCaseImpl functions namespace = T.unlines $
    [ "struct TestCase: ITestCase {" ] ++
    map (makeFunctionImpl namespace) functions ++
    [ "} testCase;" ]


makeFunctionImpl :: Text -> FunctionSignature -> Text
makeFunctionImpl namespace f = T.unlines $
    [ T.concat [ displayFunc f, " {" ]
    , T.concat $ [ "return ::dcj::", namespace, "::", makeFuncCall f ]
    , "}"
    ]

makeFuncCall :: FunctionSignature -> Text
makeFuncCall (FunctionSignature params returnValue) =
    T.concat [varName returnValue, "(", T.intercalate ", " (map varName params), ");"]

makeTestInterface :: [FunctionSignature] -> Text
makeTestInterface functions = T.unlines $
    [ "struct ITestCase {"
    , "virtual ~ITestCase() {}"
    ]
    ++ [ T.concat ["virtual ", displayFunc f, "=0;"] | f <- functions ]
    ++ [ "};" ]

displayFunc :: FunctionSignature -> Text
displayFunc (FunctionSignature params returnValue) = T.concat $
    [ displayValue returnValue
    , "("
    , T.intercalate ", " (map displayValue params)
    , ")"
    ]

displayValue :: Variable -> Text
displayValue (Variable vtype vname) = T.concat [vtype, " ", vname]

funcParser :: Parsec Text st FunctionSignature
funcParser = do
    f <- whiteSpace *> varParser
    _ <- whiteSpace >> string "(" >> whiteSpace
    params <- varParser `sepBy` (whiteSpace >> string "," >> whiteSpace)
    _ <- whiteSpace >> string ")"
    return $ FunctionSignature { funcParams = params, funcReturnValue = f }


javaLanguageDef :: P.GenLanguageDef Text st Identity
javaLanguageDef = P.LanguageDef
         { P.commentStart = P.commentStart P.javaStyle
         , P.commentEnd = P.commentEnd P.javaStyle
         , P.commentLine = P.commentLine P.javaStyle
         , P.nestedComments = P.nestedComments P.javaStyle
         , P.identStart = letter
         , P.identLetter = alphaNum <|> oneOf "_'"
         , P.opStart = P.opLetter javaLanguageDef
         , P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
         , P.reservedNames = P.reservedNames P.javaStyle
         , P.reservedOpNames = P.reservedOpNames P.javaStyle
         , P.caseSensitive = P.caseSensitive P.javaStyle
         }

lexer :: P.GenTokenParser Text st Identity
lexer = P.makeTokenParser javaLanguageDef

identifier :: Parsec Text st String
identifier = P.identifier lexer

whiteSpace :: Parsec Text st ()
whiteSpace = P.whiteSpace lexer


-- 'long long varName' -> Variable "long long" "varName"
varParser :: Parsec Text st Variable
varParser = do
    identifiers <- identifier `sepBy1` whiteSpace
    when (length identifiers <= 1) $
        fail $ "Failed to parse variable : " ++ head identifiers
    return $ Variable
            { varType = T.unwords . map T.pack . init $ identifiers
            , varName = T.pack $ last identifiers
            }

