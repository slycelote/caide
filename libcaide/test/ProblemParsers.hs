{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module ProblemParsers(
      problemParserTests
) where

import Test.HUnit
import qualified Test.HUnit as HUnit
import qualified Data.Text as T

import Caide.Parsers.Common (URL, ProblemParser(..), makeProblemParser)
import qualified Caide.Parsers.CodeChef as CodeChef
import qualified Caide.Parsers.Codeforces as Codeforces
import qualified Caide.Parsers.HackerRank as HackerRank
import qualified Caide.Parsers.POJ as POJ
import qualified Caide.Parsers.RCC as RCC
import qualified Caide.Parsers.Timus as Timus

import Caide.Types (Problem(problemName, problemId), ProblemType(Stream),
    InputSource(StdIn), OutputTarget(StdOut))
import qualified Caide.Types as Caide

assertParses :: ProblemParser -> URL -> Problem -> [Caide.TestCase] -> Test
assertParses parser url expectedProblem expectedTestCases = HUnit.TestCase $ do
    let ProblemParser{..} = parser
    assertBool "The parser must be able to handle the URL" $ problemUrlMatches url
    parseResult <- parseProblem url Nothing
    case parseResult of
        Left err -> assertFailure $ T.unpack err
        Right (problem, testCases) -> do
            assertEqual "Problem name" (problemName expectedProblem) (problemName problem)
            assertEqual "Problem ID" (problemId expectedProblem) (problemId problem)
            assertEqual "Test cases" expectedTestCases testCases

problemParserTests :: Test
problemParserTests = TestList
    [ hr "https://hackerrank.com/contests/hourrank-18/challenges/wheres-the-marble"
        (makeProblem "Where's the Marble?" "wheres-the-marble")
        [ Caide.TestCase "5 3\n2 5\n7 10\n2 9" "9" ]
    , hr "https://www.hackerrank.com/contests/projecteuler/challenges/euler254/problem"
        (makeProblem "Project Euler #254: Sums of Digit Factorials" "euler254")
        [ Caide.TestCase "2\n3 1000000\n20 1000000" "8\n156" ]
    -- Disable Equal test temporarily
    -- , hr "https://hackerrank.com/challenges/equal"
    --     (makeProblem "Equal" "equal")
    --     [ Caide.TestCase "1\n4\n2 2 3 7" "2" ]

    , chef "http://www.codechef.com/problems/MAXCIR"
        (makeProblem "Max Circumference" "MAXCIR")
        [ Caide.TestCase "3 2\n0 0\n1 0\n-1 0\n0 1\n1 0\n1 1" "6.8284271247462"
        , Caide.TestCase "3 3\n0 0\n1 0\n-1 0\n0 1\n1 0\n1 1" "7.8416192529638"
        ]
    , chef "https://www.codechef.com/problems/POWERMUL"
        (makeProblem "Fombinatorial" "POWERMUL")
        [ Caide.TestCase "1\n5 24 2\n2\n3" "8\n8" ]
    , chef "https://www.codechef.com/problems/MX"
        (makeProblem "Sorting device" "MX")
        [ Caide.TestCase "3 2\n1 2 3\n1 3 2" "2\n2 3 min\n2 3 max\n1 4 5" ]
    , chef "https://www.codechef.com/problems/LIFTME"
        (makeProblem "Lift Requests" "LIFTME")
        [ Caide.TestCase "1\n2 3\n1 2\n0 1\n1 0" "6" ]
    , chef "https://www.codechef.com/problems/DEMTREE"
        (makeProblem "Maximize Walk Value" "DEMTREE")
        [ Caide.TestCase "7 1 5\n1 1 2 2 3 3\n3 5 4 2 7 9 1\n1\n2 3 100\n1 1 100\n2 1 100\n4 5 100\n4 7 100" "6\n6\n6\n20\n16" ]

    , cf "http://codeforces.com/contest/452/problem/A"
        (makeProblem "A. Eevee" "cf452A")
        [ Caide.TestCase "7\nj......" "jolteon"
        , Caide.TestCase "7\n...feon" "leafeon"
        , Caide.TestCase "7\n.l.r.o." "flareon"
        ]
    , cf "http://codeforces.com/contest/522/problem/A?locale=ru"
        (makeProblem "A. Репосты" "cf522A")
        [ Caide.TestCase "5\ntourist reposted Polycarp\nPetr reposted Tourist\nWJMZBMR reposted Petr\nsdya reposted wjmzbmr\nvepifanov reposted sdya" "6"
        , Caide.TestCase "6\nMike reposted Polycarp\nMax reposted Polycarp\nEveryOne reposted Polycarp\n111 reposted Polycarp\nVkCup reposted Polycarp\nCodeforces reposted Polycarp" "2"
        , Caide.TestCase "1\nSoMeStRaNgEgUe reposted PoLyCaRp" "2"
        ]
    , cf "http://codeforces.com/problemset/problem/120/A"
        (makeProblem "A. Elevator" "cf120A")
        [ Caide.TestCase "front\n1" "L" ]
    , cf "http://codeforces.com/problemset/problem/120/B?locale=ru"
        (makeProblem "B. Что? Где? Когда?" "cf120B")
        [ Caide.TestCase "5 5\n0 1 0 1 0" "2"
        , Caide.TestCase "2 1\n1 1" "1"
        ]

    , poj "http://poj.org/problem?id=1067"
        (makeProblem "取石子游戏" "poj1067")
        [ Caide.TestCase "2 1\n8 4\n4 7" "0\n1\n0" ]

    , rcc "http://www.russiancodecup.ru/tasks/round/22/A/"
        (makeProblem "\"A\" Игра" "rccA")
        [ Caide.TestCase "3\n3\n1 2 3\n3 1 2\n0 2 1\n3\n1 2 3\n4 5 6\n7 8 9\n3\n1 2 3\n4 5 6\n7 5 9" "YES\nNO\nYES" ]

    , timus "http://acm.timus.ru/problem.aspx?space=1&num=2032"
        (makeProblem "2032. Conspiracy Theory and Rebranding" "timus2032")
        [ Caide.TestCase "4 3 5" "0 0\n3 4\n3 0"
        , Caide.TestCase "10 17 21" "0 0\n0 21\n-8 15"
        , Caide.TestCase "100 100 100" "-1"
        ]
    ]
  where
    hr = assertParses $ makeProblemParser HackerRank.isSupportedUrl HackerRank.htmlParser
    makeProblem name probId = Caide.makeProblem name probId (Stream StdIn StdOut)
    chef = assertParses CodeChef.problemParser
    cf = assertParses $ makeProblemParser Codeforces.isSupportedUrl Codeforces.htmlParser
    poj = assertParses $ makeProblemParser POJ.isSupportedUrl POJ.htmlParser
    rcc = assertParses $ makeProblemParser RCC.isSupportedUrl RCC.htmlParser
    timus = assertParses $ makeProblemParser Timus.isSupportedUrl Timus.htmlParser

