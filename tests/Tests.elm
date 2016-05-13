module Tests exposing (..)

import ElmTest exposing (..)
import Html
import ISO8601 exposing (DayOfWeek(..))
import ISO8601.Helpers
import Date


testTime message time year month day hour minute second millisecond offset =
    suite message
        [ time.year |> equals year
        , time.month |> equals month
        , time.day |> equals day
        , time.hour |> equals hour
        , time.minute |> equals minute
        , time.second |> equals second
        , time.millisecond |> equals millisecond
        , time.offset |> equals offset
        ]


unWrapTime : Result String ISO8601.Time -> ISO8601.Time
unWrapTime time =
    time
        |> Result.toMaybe
        |> Maybe.withDefault (ISO8601.fromTime -100000)


testParse string =
    let
        time =
            ISO8601.fromString string
    in
        testTime string (unWrapTime time)


parsingTests : Test
parsingTests =
    suite "Parsing"
        [ testParse "2006" 2006 1 1 0 0 0 0 ( 0, 0 )
        , testParse "2006-11" 2006 11 1 0 0 0 0 ( 0, 0 )
        , testParse "2015-03-02" 2015 3 2 0 0 0 0 ( 0, 0 )
        , testParse "2015-03-02T15" 2015 3 2 15 0 0 0 ( 0, 0 )
        , testParse "2015-03-02T15:16" 2015 3 2 15 16 0 0 ( 0, 0 )
        , testParse "2015-03-02T15:16:17" 2015 3 2 15 16 17 0 ( 0, 0 )
        , testParse "2006-01-02T15:04:05+00:00" 2006 1 2 15 4 5 0 ( 0, 0 )
        , testParse "2006-01-02T15:04:05+05:30" 2006 1 2 15 4 5 0 ( 5, 30 )
        , testParse "2006-01-02T15:04:05+0530" 2006 1 2 15 4 5 0 ( 5, 30 )
        , testParse "2006-01-02T15:04:05-0700" 2006 1 2 15 4 5 0 ( -7, 0 )
        , testParse "2006-01-02T15:04:05-1200" 2006 1 2 15 4 5 0 ( -12, 0 )
        , testParse "1066-12-03T10:01:59+00:00" 1066 12 3 10 1 59 0 ( 0, 0 )
          -- resoltion greater than milliseconds is rounded
        , testParse "2015-03-02T15:16:17.0009" 2015 3 2 15 16 17 1 ( 0, 0 )
        , testParse "2015-03-02T15:16:17.00049999" 2015 3 2 15 16 17 0 ( 0, 0 )
        , testParse "1066-12-03T10:01:59+00:00" 1066 12 3 10 1 59 0 ( 0, 0 )
        , testParse "1066-12-03T10:01:59.022+00:00" 1066 12 3 10 1 59 22 ( 0, 0 )
        , testParse "1066-12-03T10:01:59.5+00:00" 1066 12 3 10 1 59 500 ( 0, 0 )
          -- comma instead of period
        , testParse "1066-12-03T10:01:59,123+00:00" 1066 12 3 10 1 59 123 ( 0, 0 )
        ]


dayOfWeekTest : Test
dayOfWeekTest =
    let
        assert : String -> DayOfWeek -> Test
        assert str day =
            ISO8601.fromString str
                |> unWrapTime
                |> ISO8601.dayOfWeek
                |> equals day
    in
        suite "Day of week"
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


toUnixTest : Test
toUnixTest =
    let
        assert str seconds =
            suite str
                [ ISO8601.fromString str |> unWrapTime |> ISO8601.toTime |> equals seconds
                , ISO8601.fromString str |> unWrapTime |> ISO8601.toString |> equals str
                ]
    in
        suite "toUnix"
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
            , assert "1066-12-12T00:01:02Z" -28497657538000
            ]


testElmDateCompatibility : Test
testElmDateCompatibility =
    let
        assert str =
            let
                iso =
                    ISO8601.fromString str
                        |> unWrapTime
                        |> ISO8601.toTime

                -- round trip from ISO to ELM back to ISO
                elm =
                    iso
                        |> toFloat
                        |> Date.fromTime
                        |> Date.toTime
                        |> round
            in
                iso `equals` elm
    in
        suite "Elm.Date Compatibile"
            [ assert "1969-12-31T17:00:00-07:00"
            , assert "2016-12-31T17:00:00-07:00"
            ]


testFromUnix seconds =
    let
        time =
            ISO8601.fromTime seconds
    in
        testTime (toString seconds) time


fromUnixTest : Test
fromUnixTest =
    suite "fromTime"
        [ testFromUnix 0 1970 1 1 0 0 0 0 ( 0, 0 )
        , testFromUnix 1 1970 1 1 0 0 0 1 ( 0, 0 )
        , testFromUnix 3661123 1970 1 1 1 1 1 123 ( 0, 0 )
        , testFromUnix 86400000 1970 1 2 0 0 0 0 ( 0, 0 )
        , testFromUnix 1456707723000 2016 2 29 1 2 3 0 ( 0, 0 )
        , testFromUnix -1 1969 12 31 23 59 59 999 ( 0, 0 )
        , testFromUnix -2000 1969 12 31 23 59 58 0 ( 0, 0 )
        , testFromUnix -1456707723000 1923 11 3 22 57 57 0 ( 0, 0 )
        , testFromUnix -28497657538000 1066 12 12 0 1 2 0 ( 0, 0 )
        ]


leapYearTests =
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
            ISO8601.Helpers.isLeapYear (year) |> equals value
    in
        suite "Leap Year" (List.map assertion expectations)


errorResults =
    let
        test : String -> String -> Test
        test timeStr expected =
            case ISO8601.fromString timeStr of
                Err str ->
                    str |> equals expected

                Ok _ ->
                    "" |> equals expected
    in
        suite "errors"
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
            , test "words" "unexpected text"
            ]


rangeAssert : Int -> Int -> Int -> ( Bool, Int )
rangeAssert stop inc current =
    let
        time =
            ISO8601.fromTime current

        str =
            ISO8601.toString time
    in
        if current < stop then
            case str |> ISO8601.fromString of
                Err _ ->
                    ( False, current )

                Ok time' ->
                    if ISO8601.toTime (time') == current then
                        rangeAssert stop inc (current + inc)
                    else
                        ( False, current )
        else
            ( True, current )


testRange =
    let
        end =
            1459530703000

        inc =
            1000

        r =
            rangeAssert end 100000 (end - (1000 * 60 * 60 * 24 * 30))

        bstart =
            -312940800000

        bend =
            bstart + 1000

        b =
            rangeAssert bend 100 (bstart - 100)
    in
        suite "range"
            [ r |> equals ( True, end )
            , b |> equals ( True, bend )
            ]


all =
    suite "ISO8601"
        [ parsingTests
        , toUnixTest
        , fromUnixTest
        , leapYearTests
        , testElmDateCompatibility
        , errorResults
        , dayOfWeekTest
        , testRange
        ]


main =
    Html.pre []
        [ Html.text (stringRunner all)
        ]
