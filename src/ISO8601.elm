module ISO8601 (fromString, toTime, fromTime, toString, Time) where

{-| This package provides functionality for working with time and strings based
on the ISO 8601 standard i.e. `2016-03-31T12:13:14.22-04:00`

It is a "pure" Elm package – no Native bindings to Javascript's Date are used.
This does have a performance cost. This package is about 10x slower than the
native Date library. But there are advantages:

* _Does not cast the time in the local timezone_
* Preserves the timezone offset values
* Detects invalid dates
* Provides a record with easy access to the time's components

If you have worked with time in Javascript you may have come
across issues where the time is recast in the local timezone – not fun.

For example with Elm.Date (which uses Javascript)

````elm
Date.fromString "2016-01-01T01:30:00-04:00" \
  |> Result.map (\d -> Date.year d)
--Ok 2015 : Result.Result String Int
````

While the above is the correct time, it is looses its context – year is 2015!

Now with ISO8601
````elm
import ISO8601

t = ISO8601.fromString "2016-01-01T01:30:00-04:00"
-- Ok { year = 2016, month = 1, day = 1, hour = 1, minute = 30, second = 0, millisecond = 0, offset = (-4,0) }
    : Result.Result String ISO8601.Time

t |> Result.map .year
-- Ok 2016 : Result.Result String Int

````

ISO8601 strives to offer better error handling:
````elm
import Date
import ISO8601

-- The below is not a valid date...
Date.fromString "2014-02-30"
-- Ok {} : Result.Result String Date.Date

ISO8601.fromString "2014-02-30"
--- Err ("day is out of range") : Result.Result String ISO8601.Time
````

Error messages specify the first error found:
````elm

Date.fromString "2014-04-02T13:01:61"
-- Err ("unable to parse '2014-04-02T13:01:61' as a date")

ISO8601.fromString "2014-04-02T13:01:61"
-- Err ("second is out of range") : Result.Result String ISO8601.Time
````

# Time record
@docs Time

# Parsing
@docs fromString, toString

# Time conversion
@docs toTime, fromTime
-}

-- reading
--    http://www.oracle.com/technetwork/articles/java/jf14-date-time-2125367.html
--    https://golang.org/src/time/format.go?s=22436:22482#L722
--    https://github.com/rust-lang/rust/issues/14657
--    https://github.com/ThreeTen/threetenbp
--    https://en.wikipedia.org/wiki/ISO_8601


import Regex exposing (find, regex, split)
import String
import ISO8601.Helpers exposing (..)
import Result exposing (Result)

-- Model


type alias Offset     = ( Int, Int )

-- integeger values for periods
ims : Int
ims = 1

isec : Int
isec = ims * 1000

imin : Int
imin = isec * 60

ihour : Int
ihour = imin * 60

iday : Int
iday = ihour * 24


{-| Record representing the time. Offset is tuple representing the hour and minute ± from UTC.

-}
type alias Time =
  { year       : Int
  , month      : Int
  , day        : Int
  , hour       : Int
  , minute     : Int
  , second     : Int
  , millisecond : Int
  , offset     : (Int, Int)
  }


defaultTime : Time
defaultTime =
  { year       = 0
  , month      = 1
  , day        = 1
  , hour       = 0
  , minute     = 0
  , second     = 0
  , millisecond = 0
  , offset     = ( 0, 0 )
  }


fmt : Int -> String
fmt n =
  if n < 10 then
    "0" ++ Basics.toString n
  else
    Basics.toString n

fmtMs : Int -> String
fmtMs n =
  if n == 0 then
    ""
  else if n < 10 then
    ".00" ++ Basics.toString n
  else if n < 100 then
    ".0" ++ Basics.toString n
  else
    "." ++ Basics.toString n

fmtOffset : Offset -> String
fmtOffset offset =
  case offset of
    (0,0) -> "Z"
    (h,m) ->
      let
        symbol =
          if h >= 0  then
            "+"
          else
            "-"
      in
        symbol ++ (fmt (abs h)) ++ (fmt m)

{-| Converts a Time record to an ISO 8601 formated string.
-}
toString : Time -> String
toString time =
  String.join "" [
    fmt time.year, "-", fmt time.month, "-", fmt time.day
    , "T"
    , fmt time.hour, ":", fmt time.minute, ":",  fmt time.second
    , fmtMs time.millisecond
    , fmtOffset time.offset
    ]


{-| Given an ISO 8601 compatible string, returns a Time record.

````elm
ISO8601.fromString "2016-01-01T01:30:00-04:00"
-- { year = 2016, month = 1, day = 1, hour = 1, minute = 30, second = 0, millisecond = 0, offset = (-4,0) }
    : ISO8601.Time
ISO8601.fromString "2016-11-07"
--{ year = 2016, month = 11, day = 7, hour = 0, minute = 0, second = 0, millisecond = 0, offset = (0,0) }
    : ISO8601.Time
```

-}
fromString : String -> Result String Time
fromString s =
  -- validate the string
  -- validate the numbers
  let
    parts = List.map .submatches (iso8601Regex s)

    unwrap : Maybe String -> String -> Int
    unwrap x d =
      x |> Maybe.withDefault d |> toInt
  in
    case parts of
      [[year, month, day, hour, minute, second, millisecond, offset, invalid]] ->
        case invalid of
          Just _ -> Err "unexpected text"
          Nothing ->
            validateTime {
              year = unwrap year "0",
              month = unwrap month "1",
              day = unwrap day "1",
              hour = unwrap hour "0",
              minute = unwrap minute "0",
              second = unwrap second "0",
              -- since the ms will possibly start with 0, add the 1 and get the remainder
              -- ms' = (toInt ("1" ++ ms)) % 1000
              millisecond = parseMilliseconds millisecond,
              offset = parseOffset offset
            }
      _ ->
        let
          _ = Debug.log s parts
        in
          Err "unknown error"


iso8601Regex : String -> List Regex.Match
iso8601Regex =
  Regex.find (Regex.AtMost 1) (
    Regex.regex (
      "(\\d{4})?-?"    ++ -- year
      "(\\d{2})?-?"   ++ -- month
      "(\\d{2})?"     ++ -- DAY
      "T?"            ++ -- Time indicator
      "(\\d{2})?:?"   ++ -- hour
      "(\\d{2})?:?"   ++ -- minute
      "(\\d{2})?"     ++ -- second
      "([.,]\\d{1,})?" ++ -- fractional second
      "(Z|[+-]\\d{2}:?\\d{2})?" ++  -- offset
      "(.*)?"         -- invalid text
      )
      )

parseMilliseconds : Maybe String -> Int
parseMilliseconds msString =
  case msString of
    Nothing -> 0
    Just s ->
      let
        decimalStr = Regex.replace (Regex.AtMost 1) (Regex.regex "[,.]") (\_ -> "0.") s
        decimal = String.toFloat decimalStr |> Result.toMaybe |> Maybe.withDefault 0.0
      in
        1000 * decimal |> round


parseOffset : Maybe String -> Offset
parseOffset timeString =
  let

    re = Regex.regex "(Z|([+-]\\d{2}:?\\d{2}))?" -- offset
    -- offset can be Z or ±h:mm ±hhmm or ±hh
    match =
      timeString
        |> Maybe.withDefault ""
        |> find (Regex.AtMost 1) (regex "([-+])(\\d\\d):?(\\d\\d)")

    parts = List.map .submatches match

    setHour modifier hour =
      case modifier of
        "+" ->
          hour

        "-" ->
          modifier ++ hour

        -- this should never happen
        _ ->
          hour
  in
    case parts of
      [ [ Just modifier, Just hour, Just minute ] ] ->
        ( toInt (setHour modifier hour), toInt (minute) )

      [ [ Just modifier, Just hour ] ] ->
        ( toInt (setHour modifier hour), 0 )

      _ ->
        ( 0, 0 )


offsetToTime : Time -> Int
offsetToTime time =
  let
    ( m, s ) =
      time.offset
  in
    (ihour * m) + (imin * s)



{-| Converts the Time to milliseconds relative to the Unix epoch: `1970-01-01T00:00:00Z`
-}
toTime : Time -> Int
toTime time =
  case time.year >= 1970 of
    False ->
      let
        years =
          List.map daysInYear [(time.year + 1)..(1970 - 1)]

        totalDays =
          List.map (daysInMonth time.year) [1..(time.month)]
            |> List.sum

        tots =
          [ (iday * (List.sum years))
          , ((iday * (daysInYear time.year - totalDays)))
          , ((iday * (daysInMonth time.year time.month - time.day)))
          , (iday - ihour - (ihour * (time.hour)))
          , (ihour - imin - (imin * (time.minute)))
          , (imin - (isec * (time.second)))
          , (offsetToTime time)
          ]
      in
        0 - (List.sum tots - time.millisecond)

    True ->
      let
        years =
          List.map daysInYear [1970..(time.year - 1)]

        months =
          List.map (daysInMonth time.year) [1..(time.month - 1)]

        tots =
          [ (iday * List.sum years)
          , (iday * List.sum months)
          , (iday * (time.day - 1))
          , (ihour * (time.hour))
          , (imin * (time.minute))
          , (isec * (time.second))
          , (-1 * offsetToTime time)
          ]
      in
        (List.sum tots) + time.millisecond




{-| Converts the milliseconds relative to the Unix epoch to a Time record.
-}
fromTime : Int -> Time
fromTime ms =
  let
    milliseconds = ms % isec

    v =
      if ms >= 0 then
        After
      else
        Before
  in
    case v of
      After ->
        let

          -- additional days, the first day is implied
          days =
            ms // iday

          seconds =
            ms // isec % 60

          minutes =
            ms // imin % 60

          hours =
            ms // ihour % 24

          ( years, remaningDays ) =
            daysToYears After 1970 days

          ( month, daysInMonth ) =
            daysToMonths years 1 remaningDays

        in
          { defaultTime
            | second = seconds
            , minute = minutes
            , hour = hours
            , day = daysInMonth + 1
            , month = month
            , year = years
            , millisecond = milliseconds
          }

      Before ->
        let
          totalDays = ms // iday

          ( years, remaningDays ) =
            daysToYears Before 1969 totalDays

          ( month, daysInMonth ) =
            daysToMonths years 1 remaningDays

          rem = ms % iday

          days =
            rem // iday

          seconds =
            rem // isec % 60

          minutes =
            rem // imin % 60

          hours =
            rem // ihour % 24


        in
          { defaultTime
            | second = seconds
            , minute = minutes
            , hour = hours
            , day = daysInMonth
            , month = month
            , year = years
            , millisecond = milliseconds
          }

validateHour : Time -> Result String Time
validateHour time =
  let
      h = time.hour
      m = time.minute
      s = time.second
  in
    if h == 24 && (m+s) > 0 then
      Err "hour is out of range"
    else
      if h < 0 || h > 24 then
       Err "hour is out of range"
    else
      if m < 0 || m > 59 then
        Err "minute is out of range"
    else
      if s < 0 || s > 59 then
        Err "second is out of range"
    else
      Ok time

validateTime : Time -> Result String Time
validateTime time =
  let
    maxDays =
      daysInMonth
  in
    if time.month < 1 || time.month > 12 then
      Err "month is out of range"
    else
      if time.day < 1 || time.day > daysInMonth time.year time.month then
        Err "day is out of range"
    else
      validateHour time

