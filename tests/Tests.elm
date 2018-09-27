module Tests exposing (assertTime, fromUnixTest, rangeAssert, testDayOfWeek, testDaysToYears, testErrors, testLeapYear, testParsing, testRange, testToUnix, unWrapTime)

import Expect exposing (..)
import ISO8601 exposing (..)
import ISO8601.Extras exposing (..)
import Test exposing (..)
import Time


assertTime message time y m d h min s mil o =
    describe message
        [ test "year" <| \() -> time |> year |> equal y
        , test "month" <| \() -> time |> month |> equal m
        , test "day" <| \() -> time |> day |> equal d
        , test "hour" <| \() -> time |> hour |> equal h
        , test "minute" <| \() -> time |> minute |> equal min
        , test "second" <| \() -> time |> second |> equal s
        , test "milisecond" <| \() -> time |> millisecond |> equal mil
        , test "offset" <| \() -> time |> offset |> equal o
        ]


unWrapTime : Result String ISO8601.Time -> ISO8601.Time
unWrapTime time =
    time
        |> Result.withDefault (ISO8601.fromTime -100000)


testParsing : Test
testParsing =
    let
        assert string =
            string |> ISO8601.fromString |> unWrapTime |> assertTime string
    in
    describe "Parsing"
        [ assert "2006" 2006 1 1 0 0 0 0 ( 0, 0 )
        , assert "2006-11" 2006 11 1 0 0 0 0 ( 0, 0 )
        , assert "2015-03-02" 2015 3 2 0 0 0 0 ( 0, 0 )
        , assert "2015-03-02T15" 2015 3 2 15 0 0 0 ( 0, 0 )
        , assert "2015-03-02T15:16" 2015 3 2 15 16 0 0 ( 0, 0 )
        , assert "2015-03-02T15:16:17" 2015 3 2 15 16 17 0 ( 0, 0 )
        , assert "2006-01-02T15:04:05+00:00" 2006 1 2 15 4 5 0 ( 0, 0 )
        , assert "2006-01-02T15:04:05+05:30" 2006 1 2 15 4 5 0 ( 5, 30 )
        , assert "2006-01-02T15:04:05+0530" 2006 1 2 15 4 5 0 ( 5, 30 )
        , assert "2006-01-02T15:04:05-0700" 2006 1 2 15 4 5 0 ( -7, 0 )
        , assert "2006-01-02T15:04:05-1200" 2006 1 2 15 4 5 0 ( -12, 0 )
        , assert "1066-12-03T10:01:59+00:00" 1066 12 3 10 1 59 0 ( 0, 0 )

        -- resoltion greater than milliseconds is rounded
        , assert "2015-03-02T15:16:17.0009" 2015 3 2 15 16 17 1 ( 0, 0 )
        , assert "2015-03-02T15:16:17.00049999" 2015 3 2 15 16 17 0 ( 0, 0 )
        , assert "1066-12-03T10:01:59.022+00:00" 1066 12 3 10 1 59 22 ( 0, 0 )
        , assert "1066-12-03T10:01:59.5+00:00" 1066 12 3 10 1 59 500 ( 0, 0 )

        -- comma instead of period
        , assert "1066-12-03T10:01:59,123+00:00" 1066 12 3 10 1 59 123 ( 0, 0 )
        ]


testDaysToYears : Test
testDaysToYears =
    test "daysToYears" <| \() -> daysToYears After 1970 16801 |> equal ( 2016, 0 )


testDayOfWeek : Test
testDayOfWeek =
    let
        assert : String -> Weekday -> Test
        assert str day =
            test str <|
                \() ->
                    ISO8601.fromString str
                        |> unWrapTime
                        |> ISO8601.weekday
                        |> equal day
    in
    describe "Day of week"
        [ assert "3000-10-01" Wed
        , assert "2099-12-12" Sat
        , assert "2016-04-12" Tue
        , assert "1970-01-02" Fri
        , assert "1970-01-01" Thu
        , assert "1969-12-30" Tue
        , assert "1969-12-01" Mon
        , assert "1900-03-01" Thu
        , assert "1666-12-26" Sun

        -- no support for the Julian calendar
        , assert "1582-10-09" Sat
        , assert "1582-10-01" Fri
        , assert "0001-01-01" Mon
        ]


testToUnix : Test
testToUnix =
    let
        assert str seconds =
            describe str
                [ test "toTime" <| \() -> ISO8601.fromString str |> unWrapTime |> ISO8601.toTime |> equal seconds
                , test "toString" <| \() -> ISO8601.fromString str |> unWrapTime |> ISO8601.toString |> equal str
                ]
    in
    describe "toUnix"
        [ assert "1970-01-01T00:00:00Z" 0

        -- Unix Epoch in New Dehli
        , assert "1970-01-01T05:30:00+0530" 0
        , assert "1969-12-31T17:00:00-0700" 0
        , assert "1969-12-31T23:59:59.099Z" -901
        , assert "1969-12-31T23:59:59Z" -1000
        , assert "1969-12-31T22:59:59Z" -3601000
        , assert "1918-11-11T11:00:00Z" -1613826000000
        , assert "1918-11-11T09:00:00-0200" -1613826000000
        , assert "2016-02-04T05:06:07Z" 1454562367000
        , assert "2016-02-04T05:06:07.123Z" 1454562367123
        , assert "2016-01-01T08:06:07.123Z" 1451635567123
        , assert "1066-12-12T00:01:02Z" -28497657538000
        , assert "0001-12-12T00:01:02Z" -62105788738000
        ]


fromUnixTest : Test
fromUnixTest =
    let
        assert millseconds =
            millseconds |> ISO8601.fromTime |> assertTime (String.fromInt millseconds)
    in
    describe "fromTime"
        [ assert 0 1970 1 1 0 0 0 0 ( 0, 0 )
        , assert 1 1970 1 1 0 0 0 1 ( 0, 0 )
        , assert 3661123 1970 1 1 1 1 1 123 ( 0, 0 )
        , assert 86400000 1970 1 2 0 0 0 0 ( 0, 0 )
        , assert 1456707723000 2016 2 29 1 2 3 0 ( 0, 0 )
        , assert -1 1969 12 31 23 59 59 999 ( 0, 0 )
        , assert -2000 1969 12 31 23 59 58 0 ( 0, 0 )
        , assert -1456707723000 1923 11 3 22 57 57 0 ( 0, 0 )
        , assert -28497657538000 1066 12 12 0 1 2 0 ( 0, 0 )
        , assert 1451635200000 2016 1 1 8 0 0 0 ( 0, 0 )
        , assert 2493072000001 2049 1 1 0 0 0 1 ( 0, 0 )
        ]


testLeapYear =
    let
        expectations =
            [ ( 1804, True )
            , ( 1805, False )
            , ( 1808, True )
            , ( 1818, False )
            , ( 2015, False )
            , ( 2000, True )
            , ( 1900, False )
            , ( 1800, False )
            , ( 1600, True )
            , ( 1500, False )
            , ( 2016, True )
            ]

        assertion ( year, value ) =
            test (String.fromInt year) <|
                \() ->
                    ISO8601.Extras.isLeapYear year |> equal value
    in
    describe "Leap Year" (List.map assertion expectations)


testErrors =
    let
        test : String -> String -> Test
        test timeStr expected =
            Test.test timeStr <|
                \() ->
                    case ISO8601.fromString timeStr of
                        Err str ->
                            str |> equal expected

                        Ok _ ->
                            "" |> equal expected
    in
    describe "errors"
        [ test "2014-00-01" "month is out of range"
        , test "2014-12-00" "day is out of range"
        , test "2014-13-01" "month is out of range"
        , test "2014-02-29" "day is out of range"
        , test "2016-02-29" ""
        , test "2014-02-01T24:00:00" ""

        -- this is allowed as the interval the day transitions
        , test "2014-02-01T24:00:01" "hour is out of range"

        -- this is not
        , test "2014-02-01T12:61:01" "minute is out of range"

        -- this is not
        , test "2014-02-01T12:59:60" "second is out of range"

        -- this is not
        , test "2014-12-01 is a cow" "unexpected text"
        , test "words" "Unable to parse time"
        , Test.test "empty string" <| \() -> equal (Err "Unable to parse time") (ISO8601.fromString "")
        ]


rangeAssert : Int -> Int -> Int -> ( Bool, Int )
rangeAssert stop inc current =
    let
        time =
            ISO8601.fromTime current

        str =
            ISO8601.toString time
    in
    if current >= stop then
        ( current == stop, current )

    else
        case str |> ISO8601.fromString of
            -- there is a conversion error
            Err _ ->
                ( False, current )

            Ok time_ ->
                -- when converting back it should equal the starting value
                if ISO8601.toTime time_ == current then
                    rangeAssert stop inc (current + inc)

                else
                    ( False, current )


{-| testRange iterates by starting at a point in time and advancing by the given increment.
Along the way it tests converting from UNIX time, into a string and back,
expecting no errors and the identical final value as the starting value.
-}
testRange =
    let
        fstop =
            ISO8601.fromString "2016-04-01T00:01:00.000Z" |> unWrapTime |> ISO8601.toTime

        f =
            rangeAssert fstop
                1000
                (ISO8601.fromString "2016-03-31T23:00:00Z" |> unWrapTime |> ISO8601.toTime)

        bstop =
            ISO8601.fromString "1960-04-01T00:01:00.000Z" |> unWrapTime |> ISO8601.toTime

        b =
            rangeAssert bstop
                1000
                (ISO8601.fromString "1960-03-31T23:00:00Z" |> unWrapTime |> ISO8601.toTime)
    in
    describe "range"
        [ test "f" <| \() -> f |> equal ( True, fstop )
        , test "b" <| \() -> b |> equal ( True, bstop )
        ]
