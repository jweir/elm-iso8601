module ISO8601 (parse, toTime, fromTime) where

{-|

@docs parse, toTime, fromTime

-- reading
--    http://www.oracle.com/technetwork/articles/java/jf14-date-time-2125367.html
--    https://golang.org/src/time/format.go?s=22436:22482#L722
--    https://github.com/rust-lang/rust/issues/14657
--    https://github.com/ThreeTen/threetenbp
--    https://en.wikipedia.org/wiki/ISO_8601

-}


import Regex exposing (find, regex, split)
import String
import Array
import ISO8601.Helpers exposing (isLeapYear)
import Debug

-- Model


type alias Zone       = String
type alias Year       = Int
type alias Month      = Int
type alias Day        = Int
type alias Hour       = Int
type alias Minute     = Int
type alias Second     = Int
type alias Millisecond = Int
type alias Offset     = ( Hour, Minute )


type alias Time =
  { year       : Year
  , month      : Month
  , day        : Day
  , hour       : Hour
  , minute     : Minute
  , second     : Second
  , millisecond : Millisecond
  , offset     : Offset
  , zone       : Zone
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
  , zone       = "UTC"
  }



-- Parsing

{-| TODO Document
-}
parse : String -> Time
parse s =
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
          { date
            | hour = times.hour
            , minute = times.minute
            , second = times.second
            , millisecond = times.millisecond
            , offset = offset
          }

      [ dateString ] ->
        parseDate dateString

      _ ->
        defaultTime

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


toInt : String -> Int
toInt str =
  let
    r =
      String.toInt str
  in
    case r of
      Ok i ->
        i

      Err _ ->
        0



-- Conversion
-- name, days, leap year days


type alias CalMonth =
  ( String, Int, Int )


calendar : Array.Array CalMonth
calendar =
  Array.fromList
    [ ( "January", 31, 31 )
    , ( "February", 28, 29 )
    , ( "March", 31, 31 )
    , ( "April", 30, 30 )
    , ( "May", 31, 31 )
    , ( "June", 30, 30 )
    , ( "July", 31, 31 )
    , ( "August", 31, 31 )
    , ( "September", 30, 30 )
    , ( "October", 31, 31 )
    , ( "November", 30, 30 )
    , ( "December", 31, 31 )
    ]


-- integeger values for periods
ims : Millisecond
ims = 1

isec : Second
isec = ims * 1000

imin : Minute
imin = isec * 60

ihour : Hour
ihour = imin * 60

iday : Day
iday = ihour * 24

daysInMonth : Year -> Month -> Day
daysInMonth year monthInt =
  let
    calMonth =
      Array.get (monthInt - 1) calendar
  in
    case calMonth of
      Just ( _, days, leapDays ) ->
        if isLeapYear (year) then
          leapDays
        else
          days

      Nothing ->
        0


daysInYear : Year -> Day
daysInYear year =
  if isLeapYear (year) then
    366
  else
    365


offset : Time -> Second
offset time =
  let
    ( m, s ) =
      time.offset
  in
    (ihour * m) + (imin * s)




{-| Converts the Time to seconds starting at the Unix epoch

TODO examples

NOTE: This uses seconds, Javascript and Elm Date use milliseconds
-}
toTime : Time -> Millisecond
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
          , (offset time)
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
          , (-1 * offset time)
          ]
      in
        (List.sum tots) + time.millisecond



-- from a starting year returns the ending year and remaing days


yearsFromDays : Year -> Day -> ( Year, Day )
yearsFromDays startYear remainingDays =
  let
    remainingDays' =
      remainingDays - daysInYear startYear
  in
    if remainingDays' > 0 then
      yearsFromDays (startYear + 1) remainingDays'
    else
      ( startYear, remainingDays )

yearsFromDaysBackwards : Year -> Day -> ( Year, Day )
yearsFromDaysBackwards startYear remainingDays =
  let
    remainingDays' =
      remainingDays + daysInYear startYear
  in
    if remainingDays' < 0 then
      yearsFromDaysBackwards (startYear - 1) remainingDays'
    else
      ( startYear,  (daysInYear startYear) + remainingDays )


-- remaingDays will alawys be less than 366


monthsFromDays : Year -> Month -> Day -> ( Month, Day )
monthsFromDays year startMonth remainingDays =
  let
    remainingDays' =
      remainingDays - daysInMonth year startMonth
  in
    if remainingDays' > 0 then
      monthsFromDays year (startMonth + 1) remainingDays'
    else
      ( startMonth, remainingDays )

{-| TODO Document

NOTE: This uses seconds, Javascript and Elm Date use milliseconds
-}
fromTime : Millisecond -> Time
fromTime ms =
  let
    v =
      if ms > 0 then
        1
      else if ms < 0 then
        -1
      else
        0
  in
    case v of
      0 ->
        { defaultTime | year = 1970 }

      1 ->
        let
          milliseconds = ms % isec

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
            yearsFromDays 1970 days

          ( month, daysInMonth ) =
            monthsFromDays years 1 remaningDays

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

      -- less than UNIX epoch
      _ ->
        let
          totalDays = ms // iday

          ( years, remaningDays ) =
            yearsFromDaysBackwards 1969 totalDays

          ( month, daysInMonth ) =
            monthsFromDays years 1 remaningDays

          milliseconds = ms % isec

          rem = ms % iday

          -- additional days, the first day is implied
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

