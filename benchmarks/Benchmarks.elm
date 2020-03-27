module Benchmarks exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BlockParser exposing (parseStringWithVersion)
import Text exposing (text4)



{-

   VERSION OF MARCH 26, 2020

   text4:    3568 runs/sec @98%     : 0.3 ms/run
   text4X10: 344 runs/sec @99.83%   : 3 ms/run

   text4 has 33 lines               : 9.0 microseconds/line
   text4X10 has 339 lines           : 8.7 microseconds/line
-}


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    let
        text4X10 =
            List.repeat 10 text4 |> String.join "\n\n"

        text4X100 =
            List.repeat 100 text4 |> String.join "\n\n"
    in
    describe "BlockParser"
        [ benchmark "text4X10" <|
            \_ -> parseStringWithVersion text4X10
        ]
