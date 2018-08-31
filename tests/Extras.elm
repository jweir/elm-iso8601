module Extras exposing (a, b, testAddAndSub, testDiff, x, xx, yearAgo, z)

import Expect
import ISO8601 exposing (..)
import ISO8601.Extras as Extras
import Test exposing (Test, test)


hour : Int
hour =
    1000 * 60 * 60


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


yearAgo : Time
yearAgo =
    ISO8601.fromString "2016-01-02T00:04:05-0600" |> Result.withDefault a


testDiff : Test
testDiff =
    test "diff" <|
        \() ->
            Expect.equal -20 (diff a b)


xx =
    Debug.log "diff" (diff z yearAgo)


testAddAndSub : Test
testAddAndSub =
    let
        new =
            sub x (1000 * 60 * 60 * 3)

        -- 3 hours
    in
    Test.describe "add and sub"
        [ test "sub with diff" <|
            \() -> Expect.equal z new
        , test "sub 0" <|
            \() -> Expect.equal z (sub z 0)
        , test "sub year" <|
            \() -> Expect.equal yearAgo (sub z (hour * 24 * Extras.daysInYear 2016))
        , test "add year" <|
            \() -> Expect.equal z (add yearAgo (hour * 24 * Extras.daysInYear 2016))
        ]
