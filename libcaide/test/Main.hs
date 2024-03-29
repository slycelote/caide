{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Main where

import Test.HUnit
import System.Environment (getArgs)

import qualified Data.Aeson as Aeson

import ProblemParsers (problemParserTests)

import Caide.CodeforcesCookie (getCookie)
import Caide.Commands.ConvertTestCase (convertTopcoderParameters)
import qualified Caide.HttpClient as Http
import Caide.MustacheUtil (enrich)
import Caide.TestCases.TopcoderDeserializer (readMany, readQuotedString, readToken, runParser)
import Caide.TestCases.Types (deserializeTestReport, humanReadableReport,
    TestRunResult(testRunTime), makeTestRunResult,
    ComparisonResult(Error, EtalonUnknown, Failed, Success))
import Caide.Types (TopcoderValue(TopcoderValue), TopcoderType(TCInt, TCString))
import Caide.Util (newDefaultHttpClient)


topcoderDeserializerTests :: Test
topcoderDeserializerTests = TestLabel "topcoder-deser" $ TestList
  [ runParser readToken "a" ~?= Right "a"
  , runParser readQuotedString "\"\"" ~?= Right ""
  , runParser readQuotedString "\"abc\"" ~?= Right "abc"
  , runParser (readMany readToken) "{}" ~?= Right []
  , runParser (readMany readToken) "[] " ~?= Right []
  , runParser (readMany readToken) "{1}" ~?= Right ["1"]
  , runParser (readMany readToken) "{a, bc,ghij}" ~?= Right ["a", "bc", "ghij"]
  , runParser (readMany readToken) "[1, 2, 3]" ~?= Right ["1", "2", "3"]
  , runParser (readMany readToken) "[1, 2, 3 ]" ~?= Right ["1", "2", "3"]
  , runParser (readMany readToken) "[ 1, 2, 3 ]" ~?= Right ["1", "2", "3"]
  , runParser (readMany (readMany readToken)) "[[ 1, 2, 3 ], [4, 5]]" ~?= Right [["1", "2", "3"], ["4", "5"]]

  , runParser readToken "" ~?= Left "Offset 0: not enough input"
  , runParser readQuotedString "\"abc" ~?= Left "Offset 4: closing double quote > \": not enough input"
  , runParser (readMany readToken) "" ~?= Left "Offset 0: open bracket: not enough input"
  , runParser (readMany readToken) "[" ~?= Left "Offset 1: close bracket > ]: not enough input"
  , runParser (readMany readToken) "[1, 2, ]" ~?= Left "Offset 5: close bracket > ]: Failed reading: satisfyWith"
  , runParser (readMany readToken) "[1, 2 3]" ~?= Left "Offset 6: close bracket > ]: Failed reading: satisfyWith"
  ]


convertTestCaseInputTests :: Test
convertTestCaseInputTests = TestLabel "convert-input" $ TestList
  [ convertTopcoderParameters [] False "" ~?= Right []
  , convertTopcoderParameters [TopcoderValue "a" TCInt 0] False "1" ~?= Right ["1"]
  , convertTopcoderParameters [TopcoderValue "a" TCInt 1] False "{1, 2, 3}"
      ~?= Right ["3", "1", "2", "3"]
  , convertTopcoderParameters [TopcoderValue "a" TCInt 0, TopcoderValue "b" TCString 0] True " { 123 , \"abc\" } "
      ~?= Right ["123", "abc"]
  , convertTopcoderParameters [TopcoderValue "a" TCInt 0, TopcoderValue "b" TCString 1] False " 123 [\"abc\", \"456\" ] "
      ~?= Right ["123", "2", "abc", "456"]
  ]

testCaseSerializationTests :: Test
testCaseSerializationTests = TestList
  [ deserializeTestReport "" ~?= []
  , deserializeTestReport "  " ~?= []
  , deserializeTestReport " \r\n " ~?= []
  , deserializeTestReport "case1 error Expected 1 line(s)" ~?=
      [("case1", makeTestRunResult $ Error "Expected 1 line(s)")]
  , deserializeTestReport "case2 unknown" ~?=
      [("case2", makeTestRunResult EtalonUnknown)]
  , deserializeTestReport "case1 failed Expected    1    line(s)" ~?=
      [("case1", makeTestRunResult $ Failed "Expected    1    line(s)")]
  , deserializeTestReport "case1 error " ~?= [("case1", makeTestRunResult $ Error "")]
  , deserializeTestReport "case1 #foo:bar failed Error message" ~?=
      [("case1", makeTestRunResult $ Failed "Error message")]
  , deserializeTestReport "case1 #time:12ms OK" ~?=
      [("case1", (makeTestRunResult Success){testRunTime = Just 0.012})]
  , deserializeTestReport "case1 #time:11 OK" ~?=
      [("case1", (makeTestRunResult Success){testRunTime = Just 0.011})]
  , deserializeTestReport "case1 #time:10 #foo:bar #time:9ms OK" ~?=
      [("case1", (makeTestRunResult Success){testRunTime = Just 0.009})]
  , deserializeTestReport "case1 #time:8 failed #time:7ms error" ~?=
      [("case1", (makeTestRunResult $ Failed "#time:7ms error"){testRunTime = Just 0.008})]

  , humanReadableReport [] ~?= ""
  , humanReadableReport [ ("case1", makeTestRunResult $ Failed "error")
                        , ("case2", (makeTestRunResult Success){testRunTime = Just 0.012})
                        ] ~?=
        "case1  FAILED: error\ncase2      OK (12ms)"
  , humanReadableReport [("case1", (makeTestRunResult $ Error "message"){testRunTime = Just 0.005})] ~?=
        "case1   ERROR (5ms): message"
  ]

enrichTests :: Test
enrichTests = TestLabel "enrich" $ TestList
  [ enrich (o [("foo", n 1)]) ~?=
      o [("foo", n 1), ("foo_is_1", b True)]
  , enrich (a [n 1, n 2, n 3]) ~?= a [n 1, n 2, n 3]
  , enrich (a [o [("foo", s "#")]]) ~?=
      a [o [("foo", s "#"), ("isfirst", b True), ("islast", b True)]]
  ] where
  o = Aeson.Object
  n = Aeson.Number
  b = Aeson.Bool
  a = Aeson.Array
  s = Aeson.String


codeforcesCookieTests :: Test
codeforcesCookieTests = TestLabel "cfcookie" $ TestList
  [ getCookie "<html><body>Redirecting... Please, wait.<script type=\"text/javascript\" src=\"/aes.min.js\" ></script><script>function toNumbers(d){var e=[];d.replace(/(..)/g,function(d){e.push(parseInt(d,16))});return e}function toHex(){for(var d=[],d=1==arguments.length&&arguments[0].constructor==Array?arguments[0]:arguments,e=\"\",f=0;f<d.length;f++)e+=(16>d[f]?\"0\":\"\")+d[f].toString(16);return e.toLowerCase()}var a=toNumbers(\"e9ee4b03c1d0822987185d27bca23378\"),b=toNumbers(\"188fafdbe0f87ef0fc2810d5b3e34705\"),c=toNumbers(\"be8ee4dad93897f51910e64ea8615599\");document.cookie=\"RCPC=\"+toHex(slowAES.decrypt(c,2,a,b))+\"; expires=Thu, 31-Dec-37 23:55:55 GMT; path=/\";document.location.href=\"https://codeforces.com/?f0a28=1\";</script></body></html>"
      ~?= Right "RCPC=99b48543a1235bb6ae7d0c3a3d3c027a"
  ]

allTests :: Http.Client -> Test
allTests client = TestList
  [ topcoderDeserializerTests
  , testCaseSerializationTests
  , convertTestCaseInputTests
  , enrichTests
  , codeforcesCookieTests
  , TestLabel "live-parsers" $ problemParserTests client
  ]

filterTests :: [String] -> Test -> Test
filterTests labels t@(TestLabel label innerTest) =
    if label `elem` labels
        then t
        else TestLabel label (filterTests labels innerTest)
filterTests labels (TestList list) = TestList (map (filterTests labels) list)
filterTests _ _ = TestList []

main :: IO ()
main = do
    labels <- getArgs
    client <- newDefaultHttpClient
    let testList = allTests client
        tests = if null labels then testList else filterTests labels testList
    runTestTTAndExit tests

