module ISO8601 (fromString, toTime, fromTime, toString, Time) where

{-| This package provides functionality for working with time and strings based
on the ISO 8601 standard i.e. `2016-03-31T12:13:14.22-04:00`

It is a "pure" Elm package – no Native bindings to Javascript's Date are used.
This does have a performance cost. This package is about 10x slower than the
native Date library. But there are advantages:

* _Does not cast the time in the local timezone_
* Preserves the timezone offset values.
* Always returns a time. Invalid input results in a time of `000-01-01T00:00:00Z`

If you have worked with timestamps in a browser you may have come
across issues where the time is recast in the local timezone.

For example in Javascript

````javascript
new Date(Date.parse("2016-01-01T01:30:00-04:00"))
// Thu Dec 31 2015 21:30:00 GMT-0800 (PST)
````

While the above is the correct time, it is looses its context – if you grab the year it is 2015!

Now using ISO8601
````elm
import ISO8601

t = ISO8601.fromString "2016-01-01T01:30:00-04:00"
-- { year = 2016, month = 1, day = 1, hour = 1, minute = 30, second = 0, millisecond = 0, offset = (-4,0) }
    : ISO8601.Time

ISO8601.toString t
-- "2016-01-01T01:30:00-0400" : String

````

Example of compatibility with `Elm.Date`
````elm

import ISO8601
import Date

i = ISO8601.fromString "2016-01-01T01:30:00-04:00" |> ISO8601.toTime
-- 1451626200000 : ISO8601.Int

d = i |> toFloat |> Date.fromTime
-- {} : Date.Date
Date.year d
-- 2015 : Int
-- uh, back to our Javascript time casting problem
````


# Definition
@docs Time

# String parsing and conversion
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
    parts =
      split (Regex.All) (regex "T") s
  in
    case parts of
      [ dateString, timeString ] ->
        let
          date =
            parseDate dateString

          times =
            parseTime timeString

          offset =
            parseOffset timeString
        in
          validateTime { date
            | hour = times.hour
            , minute = times.minute
            , second = times.second
            , millisecond = times.millisecond
            , offset = offset
          }

      [ dateString ] ->
        validateTime (parseDate dateString)

      _ ->
        Err "Unparsable"

-- helper for regular expressions
matcher : Regex.Regex -> String -> List (List (Maybe String))
matcher re src =
  let
    matches =
      find (Regex.AtMost 1) re src
  in
    List.map .submatches matches


-- parses the date portion of the string
parseDate : String -> Time
parseDate dateString =
  let
    dates =
      matcher (regex "(\\d{4})-?(\\d{2})?-?(\\d{2})?") dateString
  in
    case dates of
      [ [ Just year, Just month, Just day ] ] ->
        { defaultTime | year = toInt year, month = toInt month, day = toInt day }

      [ [ Just year, Just month, Nothing ] ] ->
        { defaultTime | year = toInt year, month = toInt month }

      [ [ Just year, Nothing, Nothing ] ] ->
        { defaultTime | year = toInt year }

      _ ->
        defaultTime


-- parses the time portion of the string
parseTime : String -> Time
parseTime timeString =
  let
    times =
      -- NOTE ISO8601 states any component (hour, minute or second) supports fracational times
      -- but only one. This library only supports fractional seconds for now
      -- hh:mm:ss[.,]fractional seconds
      matcher (regex "(\\d\\d):?(\\d\\d)?:?(\\d\\d)?[.,]?(\\d{3})?") timeString
  in
    case times of
      [ [ Just hour, Just minute, Just second, Just ms ] ] ->
        let
            -- since the ms will possibly start with 0, add the 1 and get the remainder
            ms' = (toInt ("1" ++ ms)) % 1000
        in
            { defaultTime | hour = toInt hour, minute = toInt minute, second = toInt second, millisecond =  ms' }

      [ [ Just hour, Just minute, Just second, Nothing ] ] ->
        { defaultTime | hour = toInt hour, minute = toInt minute, second = toInt second }

      [ [ Just hour, Just minute, Nothing, Nothing ] ] ->
        { defaultTime | hour = toInt hour, minute = toInt minute }

      [ [ Just hour, Nothing, Nothing, Nothing ] ] ->
        { defaultTime | hour = toInt hour }

      _ ->
        defaultTime


parseOffset : String -> Offset
parseOffset timeString =
  let
    -- offset can be Z or ±h:mm ±hhmm or ±hh
    parts =
      matcher (regex "([-+])(\\d\\d):?(\\d\\d)") timeString

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

