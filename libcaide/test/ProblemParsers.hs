{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module ProblemParsers(
      problemParserTests
) where

import Test.HUnit
import qualified Test.HUnit as HUnit
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text (Text)

import qualified Caide.HttpClient as Http
import Caide.Parsers.Common (URL, ProblemParser(..), makeProblemParser)
import qualified Caide.Parsers.CodeChef as CodeChef
import qualified Caide.Parsers.Codeforces as Codeforces
import qualified Caide.Parsers.HackerRank as HackerRank
import qualified Caide.Parsers.POJ as POJ
import qualified Caide.Parsers.Timus as Timus
import qualified Caide.Parsers.LeetCode as LeetCode

import Caide.Types (Problem(problemName, problemId, problemType), ProblemType(Stream),
    InputSource(StdIn), OutputTarget(StdOut))
import Caide.Types.Option (optionFromString, optionToString)

import qualified Caide.Types as Caide


assertParses :: Http.Client -> ProblemParser -> URL -> Problem -> [Caide.TestCase] -> Test
assertParses client parser url expectedProblem expectedTestCases = HUnit.TestCase $ do
    let ProblemParser{..} = parser
    assertBool "The parser must be able to handle the URL" $ problemUrlMatches url
    parseResult <- parseProblem client url Nothing
    case parseResult of
        Left err -> assertFailure $ T.unpack $ url <> ": " <> err
        Right (problem, testCases) -> do
            assertEqual "Problem name" (problemName expectedProblem) (problemName problem)
            assertEqual "Problem ID" (problemId expectedProblem) (problemId problem)
            assertEqual "Problem type"
                (optionToString $ problemType expectedProblem)
                (optionToString $ problemType problem)
            assertEqual "Test cases" expectedTestCases testCases

mkTestCase :: Text -> Text -> Caide.TestCase
mkTestCase i o = Caide.TestCase i (Just o)

mkProblemType :: String -> ProblemType
mkProblemType = fromJust . optionFromString

mkTestList :: String -> [Test] -> Test
mkTestList label tests = TestLabel label $ TestList tests

problemParserTests :: Http.Client -> Test
problemParserTests client = TestList
    [ hr "https://hackerrank.com/contests/hourrank-18/challenges/wheres-the-marble"
        (makeProblem "Where's the Marble?" "wheres-the-marble")
        [ mkTestCase "5 3\n2 5\n7 10\n2 9" "9" ]
    , hr "https://www.hackerrank.com/contests/projecteuler/challenges/euler254/problem"
        (makeProblem "Project Euler #254: Sums of Digit Factorials" "euler254")
        [ mkTestCase "2\n3 1000000\n20 1000000" "8\n156" ]
    -- Disable Equal test temporarily
    -- , hr "https://hackerrank.com/challenges/equal"
    --     (makeProblem "Equal" "equal")
    --     [ mkTestCase "1\n4\n2 2 3 7" "2" ]

    , mkTestList "codechef"
        [ chef "http://www.codechef.com/problems/MAXCIR"
            (makeProblem "Max Circumference" "MAXCIR")
            [ mkTestCase "3 2\n0 0\n1 0\n-1 0\n0 1\n1 0\n1 1" "6.8284271247462"
            , mkTestCase "3 3\n0 0\n1 0\n-1 0\n0 1\n1 0\n1 1" "7.8416192529638"
            ]
        , chef "https://www.codechef.com/problems/POWERMUL"
            (makeProblem "Fombinatorial" "POWERMUL")
            [ mkTestCase "1\n5 24 2\n2\n3" "8\n8" ]
        , chef "https://www.codechef.com/problems/MX"
            (makeProblem "Sorting device" "MX")
            [ mkTestCase "3 2\n1 2 3\n1 3 2" "2\n2 3 min\n2 3 max\n1 4 5" ]
        , chef "https://www.codechef.com/problems/LIFTME"
            (makeProblem "Lift Requests" "LIFTME")
            [ mkTestCase "1\n2 3\n1 2\n0 1\n1 0" "6" ]
        , chef "https://www.codechef.com/problems/DEMTREE"
            (makeProblem "Maximize Walk Value" "DEMTREE")
            [ mkTestCase "7 1 5\n1 1 2 2 3 3\n3 5 4 2 7 9 1\n1\n2 3 100\n1 1 100\n2 1 100\n4 5 100\n4 7 100" "6\n6\n6\n20\n16" ]
        , chef "https://www.codechef.com/SNCKQL21/problems/LUCKYNUM"
            (makeProblem "Lucky Number" "LUCKYNUM")
            [ mkTestCase "3\n0 0 0\n7 8 9\n2 7 7" "NO\nYES\nYES" ]
        , chef "https://www.codechef.com/SNCKQL21/problems/TESTSERIES"
            (makeProblem "Test Match Series" "TESTSERIES")
            [ mkTestCase "3\n0 1 2 1 0\n0 1 2 1 2\n2 2 2 2 1" "INDIA\nDRAW\nENGLAND" ]
        ]

    , mkTestList "codeforces"
        [ cf "http://codeforces.com/contest/452/problem/A"
            (makeProblem "A. Eevee" "cf452A")
            [ mkTestCase "7\nj......" "jolteon"
            , mkTestCase "7\n...feon" "leafeon"
            , mkTestCase "7\n.l.r.o." "flareon"
            ]
        , cf "http://codeforces.com/contest/522/problem/A?locale=ru"
            (makeProblem "A. Репосты" "cf522A")
            [ mkTestCase "5\ntourist reposted Polycarp\nPetr reposted Tourist\nWJMZBMR reposted Petr\nsdya reposted wjmzbmr\nvepifanov reposted sdya" "6"
            , mkTestCase "6\nMike reposted Polycarp\nMax reposted Polycarp\nEveryOne reposted Polycarp\n111 reposted Polycarp\nVkCup reposted Polycarp\nCodeforces reposted Polycarp" "2"
            , mkTestCase "1\nSoMeStRaNgEgUe reposted PoLyCaRp" "2"
            ]
        , cf "http://codeforces.com/problemset/problem/120/A"
            (makeProblem "A. Elevator" "cf120A")
            { problemType = mkProblemType "file,input.txt,output.txt" }
            [ mkTestCase "front\n1" "L" ]
        , cf "http://codeforces.com/problemset/problem/120/B?locale=ru"
            (makeProblem "B. Что? Где? Когда?" "cf120B")
            { problemType = mkProblemType "file,input.txt,output.txt" }
            [ mkTestCase "5 5\n0 1 0 1 0" "2"
            , mkTestCase "2 1\n1 1" "1"
            ]
        ]

    , poj "http://poj.org/problem?id=1067"
        (makeProblem "取石子游戏" "poj1067")
        [ mkTestCase "2 1\n8 4\n4 7" "0\n1\n0" ]

    , timus "http://acm.timus.ru/problem.aspx?space=1&num=2032"
        (makeProblem "2032. Conspiracy Theory and Rebranding" "timus2032")
        [ mkTestCase "4 3 5" "0 0\n3 4\n3 0"
        , mkTestCase "10 17 21" "0 0\n0 21\n-8 15"
        , mkTestCase "100 100 100" "-1"
        ]

    , mkTestList "leetcode"
        [ lc "https://leetcode.com/problems/longest-common-prefix"
            (makeProblem "Longest Common Prefix" "longest-common-prefix")
            { problemType = mkProblemType "leetcode,longestCommonPrefix:String,strs:vString" }
            [ mkTestCase "[\"flower\",\"flow\",\"flight\"]" "\"fl\""
            , mkTestCase "[\"dog\",\"racecar\",\"car\"]" "\"\""
            ]
        , lc "https://leetcode.com/problems/median-of-two-sorted-arrays"
            (makeProblem "Median of Two Sorted Arrays" "median-of-two-sorted-arrays")
            { problemType = mkProblemType "leetcode,findMedianSortedArrays:double,nums1:vint,nums2:vint" }
            [ mkTestCase "[1,3]\n[2]" "2.00000"
            , mkTestCase "[1,2]\n[3,4]" "2.50000"
            ]
        , lc "https://leetcode.com/problems/sequential-digits"
            (makeProblem "Sequential Digits" "sequential-digits")
            { problemType = mkProblemType "leetcode,sequentialDigits:vint,low:int,high:int" }
            [ mkTestCase "100\n300" "[123,234]"
            , mkTestCase "1000\n13000" "[1234,2345,3456,4567,5678,6789,12345]"
            ]
        , lc "https://leetcode.com/problems/detect-capital"
            (makeProblem "Detect Capital" "detect-capital")
            { problemType = mkProblemType "leetcode,detectCapitalUse:bool,word:String" }
            [ mkTestCase "\"USA\"" "true"
            , mkTestCase "\"FlaG\"" "false"
            ]
        , lc "https://leetcode.com/problems/k-highest-ranked-items-within-a-price-range"
            (makeProblem "K Highest Ranked Items Within a Price Range" "k-highest-ranked-items-within-a-price-range")
            { problemType = mkProblemType "leetcode,highestRankedKItems:vvint,grid:vvint,pricing:vint,start:vint,k:int" }
            [ mkTestCase "[[1,2,0,1],[1,3,0,1],[0,2,5,1]]\n[2,5]\n[0,0]\n3" "[[0,1],[1,1],[2,1]]"
            , mkTestCase "[[1,2,0,1],[1,3,3,1],[0,2,5,1]]\n[2,3]\n[2,3]\n2" "[[2,1],[1,2]]"
            , mkTestCase "[[1,1,1],[0,0,1],[2,3,4]]\n[2,3]\n[0,0]\n3" "[[2,1],[2,0]]"
            ]
        , lc "https://leetcode.com/problems/add-two-numbers"
            (makeProblem "Add Two Numbers" "add-two-numbers")
            { problemType = mkProblemType "leetcode,addTwoNumbers:ListNode,l1:ListNode,l2:ListNode" }
            [ mkTestCase "[2,4,3]\n[5,6,4]" "[7,0,8]"
            , mkTestCase "[0]\n[0]" "[0]"
            , mkTestCase "[9,9,9,9,9,9,9]\n[9,9,9,9]" "[8,9,9,9,0,0,0,1]"
            ]
        , lc "https://leetcode.com/problems/lru-cache"
            (makeProblem "LRU Cache" "lru-cache")
            { problemType = mkProblemType "leetcode;LRUCache,capacity:int;get:int,key:int;put:void,key:int,value:int" }
            [ mkTestCase "[\"LRUCache\",\"put\",\"put\",\"get\",\"put\",\"get\",\"put\",\"get\",\"get\",\"get\"]\n[[2],[1,1],[2,2],[1],[3,3],[2],[4,4],[1],[3],[4]]"
                         "[null, null, null, 1, null, -1, null, -1, 3, 4]"
            ]
        , lc "https://leetcode.com/contest/weekly-contest-291/problems/total-appeal-of-a-string"
            (makeProblem "Total Appeal of A String" "total-appeal-of-a-string")
            { problemType = mkProblemType "leetcode,appealSum:long,s:string" }
            [ mkTestCase "\"abbca\"" "28"
            , mkTestCase "\"code\"" "20"
            ]
        ]
    ]
  where
    hr = assertParses client $ makeProblemParser HackerRank.isSupportedUrl HackerRank.htmlParser
    makeProblem name probId = Caide.makeProblem name probId (Stream StdIn StdOut)
    chef = assertParses client CodeChef.problemParser
    cf = assertParses client $ makeProblemParser Codeforces.isSupportedUrl Codeforces.htmlParser
    poj = assertParses client $ makeProblemParser POJ.isSupportedUrl POJ.htmlParser
    timus = assertParses client $ makeProblemParser Timus.isSupportedUrl Timus.htmlParser
    lc = assertParses client LeetCode.problemParser

