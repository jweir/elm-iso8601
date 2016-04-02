module Tests (..) where

import ElmTest exposing (..)
import ISO8601 
import ISO8601.Helpers

testTime time year month day hour minute second millisecond offset zone =
  suite
    ""
    [ time.year |> equals year
    , time.month |> equals month
    , time.day |> equals day
    , time.hour |> equals hour
    , time.minute |> equals minute
    , time.second |> equals second
    , time.millisecond |> equals millisecond
    , time.offset |> equals offset
    , time.zone |> equals zone
    ]


testParse string =
  let
    time =
      ISO8601.parse string
  in
    testTime time


parsingTests : Test
parsingTests =
    suite
        "Parsing"
        [ testParse "2006" 2006 1 1 0 0 0 0 ( 0, 0 ) "UTC"
        , testParse "2006-11" 2006 11 1 0 0 0 0 ( 0, 0 ) "UTC"
        , testParse "2015-03-02" 2015 3 2 0 0 0 0 ( 0, 0 ) "UTC"
        , testParse "2015-03-02T15" 2015 3 2 15 0 0 0 ( 0, 0 ) "UTC"
        , testParse "2015-03-02T15:16" 2015 3 2 15 16 0 0 ( 0, 0 ) "UTC"
        , testParse "2015-03-02T15:16:17" 2015 3 2 15 16 17 0 ( 0, 0 ) "UTC"
        , testParse "2006-01-02T15:04:05+00:00" 2006 1 2 15 4 5 0 ( 0, 0 ) "UTC"
        , testParse "2006-01-02T15:04:05+05:30" 2006 1 2 15 4 5 0 ( 5, 30 ) "UTC"
        , testParse "2006-01-02T15:04:05-0700" 2006 1 2 15 4 5 0 ( -7, 0 ) "UTC"
        , testParse "1066-12-03T10:01:59+00:00" 1066 12 3 10 1 59 0 ( 0, 0 ) "UTC"
        , testParse "1066-12-03T10:01:59.022+00:00" 1066 12 3 10 1 59 22 ( 0, 0 ) "UTC"
        , testParse "1066-12-03T10:01:59,123+00:00" 1066 12 3 10 1 59 123 ( 0, 0 ) "UTC"
        ]


toUnixTest : Test
toUnixTest =
  let
    assert str seconds =
      ISO8601.parse str
        |> ISO8601.toUnix
        |> equals seconds
  in
    suite
      "toUnix"
      [ assert "1970-01-01T00:00:00Z" 0
        -- Unix Epoch in New Dehli
      , assert "1970-01-01T05:30:00+05:30" 0
      , assert "1969-12-31T17:00:00-07:00" 0
      , assert "1969-12-31T23:59:59Z" -1
      , assert "1969-12-31T22:59:59Z" -3601
      , assert "1918-11-11T11:00:00Z" -1613826000
      , assert "1918-11-11T09:00:00-0200" -1613826000
      , assert "2016-02-04T05:06:07Z" 1454562367
      ]


testFromUnix seconds =
  let
    time =
      ISO8601.fromUnix seconds
  in
    testTime time


fromUnixTest : Test
fromUnixTest =
  suite
    "fromUnix"
    [ testFromUnix 0 1970 1 1 0 0 0 0 ( 0, 0 ) "UTC"
    , testFromUnix 3661 1970 1 1 1 1 1 0 ( 0, 0 ) "UTC"
    , testFromUnix 86400 1970 1 2 0 0 0 0 ( 0, 0 ) "UTC"
    , testFromUnix 1456707723 2016 2 29 1 2 3 0 ( 0, 0 ) "UTC"
    ]


leapYearTests =
  let
    expectations =
      [ ( 1804, True )
      , ( 1805, False )
      , ( 1808, True )
      , ( 1818, False )
      , ( 2015, False )
      , ( 2016, True )
      ]

    assertion ( year, value ) =
      ISO8601.Helpers.isLeapYear (year) |> equals value
  in
    suite "Leap Year" (List.map assertion expectations)


main =
  elementRunner (suite "ISO8601" [ parsingTests, toUnixTest, fromUnixTest, leapYearTests ])
