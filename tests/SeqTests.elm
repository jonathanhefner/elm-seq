module SeqTests where

import ElmTest.Test (test, Test, suite)
import ElmTest.Assertion (assert, assertEqual)
import ElmTest.Runner.Element (runDisplay)

import Seq (..)
import List


main = runDisplay <| suite "Seq tests"
  [ testSeqOfN 0
  , testSeqOfN 1
  , testSeqOfN 1000
  ]


testSeqOfN : Int -> Test
testSeqOfN n =
  let xs = [1..n]
      xq = fromList xs
      zs = [0..n]
      zq = fromList zs
      xsSum = n * (n + 1) // 2
  in
      suite (toString n ++ " elements")
        [ test "toList" <| assertEqual (xs) (toList xq)

        , suite "fold"
            [ test "order" <| assertEqual (n) (fold (\x acc -> x) 0 xq)
            , test "total" <| assertEqual (xsSum) (fold (+) 0 xq)
            ]

        , test "map" <| assertEqual ([2..(n + 1)]) (map ((+) 1) xq |> toList)

        , suite "keep"
            [ test "none" <| assertEqual ([]) (keep 0 xq |> toList)
            , test "some" <| assertEqual ([0..(n - 1)]) (keep n zq |> toList)
            , test "all" <| assertEqual (xs) (keep n xq |> toList)
            , test "all+" <| assertEqual (xs) (keep (n + 1) xq |> toList)
            ]

        , suite "drop"
            [ test "none" <| assertEqual (xs) (drop 0 xq |> toList)
            , test "some" <| assertEqual ([n]) (drop n zq |> toList)
            , test "all" <| assertEqual ([]) (drop n xq |> toList)
            , test "all+" <| assertEqual ([]) (drop (n + 1) xq |> toList)
            ]

        , suite "keepIf"
            [ test "none" <| assertEqual ([]) (keepIf (\x -> x > n) xq |> toList)
            , test "one" <| assertEqual ([n]) (keepIf (\z -> z == n) zq |> toList)
            , test "all" <| assertEqual (xs) (keepIf (\x -> x <= n) xq |> toList)
            ]
        ]
