module Calc exposing (..)

import Test exposing (Test, test)
import Expect
import ISO8601 exposing (..)
import ISO8601.Math as Math


a : Time
a =
    ISO8601.fromTime 0


b : Time
b =
    ISO8601.fromTime 20


x : Time
x =
    ISO8601.fromString "2017-01-02T03:04:05-0600" |> Result.withDefault a


z : Time
z =
    ISO8601.fromString "2017-01-02T00:04:05-0600" |> Result.withDefault a


testDiff : Test
testDiff =
    test "diff" <|
        \() ->
            Expect.equal -20 (Math.diff a b)


testSub : Test
testSub =
    let
        new =
            Math.sub x (1000 * 60 * 60 * 3)

        -- 3 hours
    in
        Test.describe "sub"
            [ test "sub with diff" <|
                \() -> Expect.equal z new
            , test "sub 0" <|
                \() -> Expect.equal z (Math.sub z 0)
            ]
