module Benchmarks exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BlockParser exposing (getNodeFromTree, parseStringWithVersion, sourceMapFromTree)
import Text exposing (text4)


main : BenchmarkProgram
main =
    program suite1


{-|

    text4:    3568 runs/sec     : 0.3 ms/run
    text4X10:  344 runs/sec     : 3 ms/run

    text4      has 33 lines     : 9.0 microseconds/line
    text4X10  has 339 lines     : 8.7 microseconds/line

-}
suite1 : Benchmark
suite1 =
    let
        text4X10 =
            List.repeat 10 text4 |> String.join "\n\n"

        text4X100 =
            List.repeat 100 text4 |> String.join "\n\n"
    in
    describe "BlockParser"
        [ benchmark "text4X10" <|
            \_ -> parseStringWithVersion 0 text4X10
        ]


{-| On text4:

    sourceMapFromTree    73,866 runs/sec   : 13.4 microseconds
    getNode             150,149 rus/sec    :  6.7 microseconds

-}
suite2 : Benchmark
suite2 =
    let
        bt =
            parseStringWithVersion 0 text4

        sourceMap =
            sourceMapFromTree bt
    in
    describe "BlockParser II"
        [ benchmark "sourceMapFromTree" <|
            \_ -> sourceMapFromTree bt
        , benchmark "getNode" <|
            \_ -> getNodeFromTree ( 0, 14 ) bt
        ]


{-| On text4X5:

     sourceMapFromTree    15,389 runs/sec   :  65.0 microseconds
     getNode             131,359 rus/sec    :   7.6 microseconds

-}
suite3 : Benchmark
suite3 =
    let
        text4X5 =
            (List.repeat 5 text4 |> String.join "\n\n") ++ "\n\n| section Conclusion"

        bt =
            parseStringWithVersion 0 text4X5

        sourceMap =
            sourceMapFromTree bt
    in
    describe "BlockParser III"
        [ benchmark "sourceMapFromTree" <|
            \_ -> sourceMapFromTree bt
        , benchmark "getNode" <|
            \_ -> getNodeFromTree ( 0, 70 ) bt
        ]


{-| On text4X10:

     sourceMapFromTree    7489 runs/sec   :  134 microseconds
     getNode             16719 runs/sec    :  60 microseconds

-}
suite4 : Benchmark
suite4 =
    let
        text4X10 =
            (List.repeat 10 text4 |> String.join "\n\n") ++ "\n\n| section Conclusion"

        bt =
            parseStringWithVersion 0 text4X10

        sourceMap =
            sourceMapFromTree bt
    in
    describe "BlockParser IV"
        [ benchmark "sourceMapFromTree" <|
            \_ -> sourceMapFromTree bt
        , benchmark "getNode" <|
            \_ -> getNodeFromTree ( 0, 140 ) bt
        ]


{-| On text4X100:

     sourceMapFromTree    686 runs/sec   :  1.5 milliseconds  (11.2 x text4X10)
     getNode             2044 runs/sec   :  0.5 milliseconds   (8.3 x text410)

     So the time complexity appears to be roughly linear in the size of the source
     as measured by number of lines.

-}
suite5 : Benchmark
suite5 =
    let
        text4X100 =
            (List.repeat 10 text4 |> String.join "\n\n") ++ "\n\n| section Conclusion"

        bt =
            parseStringWithVersion 0 text4X100

        sourceMap =
            sourceMapFromTree bt
    in
    describe "BlockParser IV"
        [ benchmark "sourceMapFromTree" <|
            \_ -> sourceMapFromTree bt
        , benchmark "getNode" <|
            \_ -> getNodeFromTree ( 0, 140 ) bt
        ]
