module ISO8601 exposing
    ( Time, Offset
    , year, month, day, hour, minute, second, millisecond, offset, weekday
    , fromString, toString
    , toTime, fromTime, toPosix, fromPosix
    , diff, sub, add
    )

{-| This package provides functionality for working with time and strings based
on the ISO 8601 standard i.e. `2016-03-31T12:13:14.22-04:00`


# Time record

@docs Time, Offset


# Accessors

@docs year, month, day, hour, minute, second, millisecond, offset, weekday


# Parsing

@docs fromString, toString


# Time conversion

@docs toTime, fromTime, toPosix, fromPosix


# Manipulation

Note: The Time record has an offset, but not a time size. Adding or
subtracting time across a daylight savings boundary will NOT adjust the
offset.

@docs diff, sub, add

-}

import Array
import ISO8601.Extras exposing (..)
import Regex exposing (find, replace, split)
import Result exposing (Result)
import String
import Time exposing (Posix(..), Weekday(..))



-- Model


{-| Offset represents the hour and minute timezone offset from UTC.
-}
type alias Offset =
    ( Int, Int )



-- integeger values for periods


ims : Int
ims =
    1


isec : Int
isec =
    ims * 1000


imin : Int
imin =
    isec * 60


ihour : Int
ihour =
    imin * 60


iday : Int
iday =
    ihour * 24


{-| Record representing the time. Offset is tuple representing the hour and minute ± from UTC.
-}
type alias Time =
    Model


type alias Model =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    , minute : Int
    , second : Int
    , millisecond : Int
    , offset : ( Int, Int )
    }


defaultTime : Time
defaultTime =
    { year = 0
    , month = 1
    , day = 1
    , hour = 0
    , minute = 0
    , second = 0
    , millisecond = 0
    , offset = ( 0, 0 )
    }


fmt : Int -> String
fmt n =
    if n < 10 then
        "0" ++ String.fromInt n

    else
        String.fromInt n


fmtYear : Int -> String
fmtYear n =
    let
        s =
            String.fromInt n
    in
    if n < 10 then
        "000" ++ s

    else if n < 100 then
        "00" ++ s

    else if n < 1000 then
        "0" ++ s

    else
        s


fmtMs : Int -> String
fmtMs n =
    if n == 0 then
        ""

    else if n < 10 then
        ".00" ++ String.fromInt n

    else if n < 100 then
        ".0" ++ String.fromInt n

    else
        "." ++ String.fromInt n


fmtOffset : Offset -> String
fmtOffset o =
    case o of
        ( 0, 0 ) ->
            "Z"

        ( h, m ) ->
            let
                symbol =
                    if h >= 0 then
                        "+"

                    else
                        "-"
            in
            symbol ++ fmt (abs h) ++ fmt m


{-| Converts a Time record to an ISO 8601 formated string.
-}
toString : Time -> String
toString time =
    String.join ""
        [ fmtYear time.year
        , "-"
        , fmt time.month
        , "-"
        , fmt time.day
        , "T"
        , fmt time.hour
        , ":"
        , fmt time.minute
        , ":"
        , fmt time.second
        , fmtMs time.millisecond
        , fmtOffset time.offset
        ]


{-| Given an ISO 8601 compatible string, returns a Time record.

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
            List.map .submatches (iso8601Regex s)

        unwrap : Maybe String -> String -> Int
        unwrap x d =
            x |> Maybe.withDefault d |> toInt
    in
    case parts of
        [ [ y, mon, d, h, min, sec, mil, off, invalid ] ] ->
            case invalid of
                Just _ ->
                    Err "unexpected text"

                Nothing ->
                    validateTime
                        { year = unwrap y "0"
                        , month = unwrap mon "1"
                        , day = unwrap d "1"
                        , hour = unwrap h "0"
                        , minute = unwrap min "0"
                        , second = unwrap sec "0"
                        , -- since the ms will possibly start with 0, add the 1 and get the remainder
                          -- ms' = (toInt ("1" ++ ms)) % 1000
                          millisecond = parseMilliseconds mil
                        , offset = parseOffset off
                        }

        _ ->
            Err "Unable to parse time"


iso8601Regex : String -> List Regex.Match
iso8601Regex =
    Regex.findAtMost 1
        (Regex.fromString
            ("(\\d{4})-?"
                ++ -- year
                   "(\\d{2})?-?"
                ++ -- month
                   "(\\d{2})?"
                ++ -- DAY
                   "T?"
                ++ -- Time indicator
                   "(\\d{2})?:?"
                ++ -- hour
                   "(\\d{2})?:?"
                ++ -- minute
                   "(\\d{2})?"
                ++ -- second
                   "([.,]\\d{1,})?"
                ++ -- fractional second
                   "(Z|[+-]\\d{2}:?\\d{2})?"
                ++ -- offset
                   "(.*)?"
             -- invalid text
            )
            |> Maybe.withDefault Regex.never
        )


parseMilliseconds : Maybe String -> Int
parseMilliseconds msString =
    case msString of
        Nothing ->
            0

        Just s ->
            let
                decimalStr =
                    Regex.replaceAtMost 1
                        (Regex.fromString "[,.]"
                            |> Maybe.withDefault Regex.never
                        )
                        (\_ -> "0.")
                        s

                decimal =
                    String.toFloat decimalStr |> Maybe.withDefault 0.0
            in
            1000 * decimal |> round


parseOffset : Maybe String -> Offset
parseOffset timeString =
    let
        re =
            Regex.fromString "(Z|([+-]\\d{2}:?\\d{2}))?" |> Maybe.withDefault Regex.never

        -- offset
        -- offset can be Z or ±h:mm ±hhmm or ±hh
        match =
            timeString
                |> Maybe.withDefault ""
                |> Regex.findAtMost 1 (Regex.fromString "([-+])(\\d\\d):?(\\d\\d)" |> Maybe.withDefault Regex.never)

        parts =
            List.map .submatches match

        setHour modifier hour_ =
            case modifier of
                "+" ->
                    hour_

                "-" ->
                    modifier ++ hour_

                -- this should never happen
                _ ->
                    hour_
    in
    case parts of
        [ [ Just modifier, Just hour_, Just minute_ ] ] ->
            ( toInt (setHour modifier hour_), toInt minute_ )

        [ [ Just modifier, Just hour_ ] ] ->
            ( toInt (setHour modifier hour_), 0 )

        _ ->
            ( 0, 0 )


offsetToTime : Time -> Int
offsetToTime time =
    let
        ( m, s ) =
            time.offset
    in
    (ihour * m) + (imin * s)


{-| Converts the Time to standard Time.Posix type
-}
toPosix : Time -> Posix
toPosix time =
    toTime time |> Time.millisToPosix


{-| Converts the Time.Posix type to an ISO8601.Time
-}
fromPosix : Posix -> Time
fromPosix posix =
    posix
        |> Time.posixToMillis
        |> fromTime


{-| Converts the Time to milliseconds relative to the Unix epoch: `1970-01-01T00:00:00Z`
-}
toTime : Time -> Int
toTime time =
    case time.year >= 1970 of
        False ->
            let
                years =
                    List.map daysInYear (List.range (time.year + 1) (1970 - 1))

                totalDays =
                    List.map (daysInMonth time.year) (List.range 1 time.month)
                        |> List.sum

                tots =
                    [ iday * List.sum years
                    , iday * (daysInYear time.year - totalDays)
                    , iday * (daysInMonth time.year time.month - time.day)
                    , iday - ihour - (ihour * time.hour)
                    , ihour - imin - (imin * time.minute)
                    , imin - (isec * time.second)
                    , offsetToTime time
                    ]
            in
            0 - (List.sum tots - time.millisecond)

        True ->
            let
                years =
                    List.map daysInYear (List.range 1970 (time.year - 1))

                months =
                    List.map (daysInMonth time.year) (List.range 1 (time.month - 1))

                tots =
                    [ iday * List.sum years
                    , iday * List.sum months
                    , iday * (time.day - 1)
                    , ihour * time.hour
                    , imin * time.minute
                    , isec * time.second
                    , -1 * offsetToTime time
                    ]
            in
            List.sum tots + time.millisecond


{-| Converts the milliseconds relative to the Unix epoch to a Time record.
-}
fromTime : Int -> Time
fromTime msFloat =
    let
        ms =
            msFloat

        milliseconds =
            ms |> modBy isec

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

                -- the int might be too large here to safely calculate in javascript
                -- so reduce the size first
                seconds =
                    (ms |> toFloat) / (isec |> toFloat) |> round |> modBy 60

                minutes =
                    ms // imin |> modBy 60

                hours =
                    ms // ihour |> modBy 24

                ( years, remainingDays ) =
                    daysToYears After 1970 days

                ( months, daysInMonth ) =
                    daysToMonths years 1 (remainingDays + 1)
            in
            { defaultTime
                | second = seconds
                , minute = minutes
                , hour = hours
                , day = daysInMonth
                , month = months
                , year = years
                , millisecond = milliseconds
            }

        Before ->
            let
                rem =
                    ms |> modBy iday

                totalDays =
                    ms // iday

                -- this is right at the start of a new day
                ( years, remainingDays ) =
                    if rem == 0 then
                        daysToYears Before 1969 (totalDays + 1)

                    else
                        daysToYears Before 1969 totalDays

                ( months, daysInMonth ) =
                    daysToMonths years 1 remainingDays

                days =
                    rem // iday

                seconds =
                    rem // isec |> modBy 60

                minutes =
                    rem // imin |> modBy 60

                hours =
                    rem // ihour |> modBy 24
            in
            { defaultTime
                | second = seconds
                , minute = minutes
                , hour = hours
                , day = daysInMonth
                , month = months
                , year = years
                , millisecond = milliseconds
            }


validateHour : Time -> Result String Time
validateHour time =
    let
        h =
            time.hour

        m =
            time.minute

        s =
            time.second
    in
    if h == 24 && (m + s) > 0 then
        Err "hour is out of range"

    else if h < 0 || h > 24 then
        Err "hour is out of range"

    else if m < 0 || m > 59 then
        Err "minute is out of range"

    else if s < 0 || s > 59 then
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

    else if time.day < 1 || time.day > daysInMonth time.year time.month then
        Err "day is out of range"

    else
        validateHour time


{-| return the year
-}
year : Time -> Int
year time =
    time.year


{-| return the month
-}
month : Time -> Int
month time =
    time.month


{-| return the day
-}
day : Time -> Int
day time =
    time.day


{-| return the hour
-}
hour : Time -> Int
hour time =
    time.hour


{-| return the minute
-}
minute : Time -> Int
minute time =
    time.minute


{-| return the secon
-}
second : Time -> Int
second time =
    time.second


{-| return the millisecond
-}
millisecond : Time -> Int
millisecond time =
    time.millisecond


{-| return the offset
-}
offset : Time -> Offset
offset time =
    time.offset


{-| Returns the day of the week from the Time record
-}
weekday : Time -> Time.Weekday
weekday time =
    let
        daysFromEpoch =
            Array.fromList [ Thu, Fri, Sat, Sun, Mon, Tue, Wed ]

        daysSinceEpoch =
            ({ defaultTime
                | year = time.year
                , month = time.month
                , day = time.day
             }
                |> toTime
            )
                // iday

        days =
            Array.get (daysSinceEpoch |> modBy 7) daysFromEpoch
    in
    case days of
        Just d ->
            d

        -- this should never be reached
        Nothing ->
            Sun



-- Manipulation


{-| the difference between two Time records in milliseconds
-}
diff : Time -> Time -> Int
diff a b =
    toTime a - toTime b


{-| Subtract milliseconds from a Time records
-}
sub : Time -> Int -> Time
sub time amount =
    toTime time - amount |> fromTimeWithOffset time.offset


{-| Add milliseconds to a Time records
-}
add : Time -> Int -> Time
add time amount =
    toTime time + amount |> fromTimeWithOffset time.offset


offsetToMS : Offset -> Int
offsetToMS offsets =
    let
        ( hours, minutes ) =
            offsets
    in
    (hours * 60 * 60 * 1000) + (minutes * 60 * 1000)


fromTimeWithOffset : Offset -> Int -> Time
fromTimeWithOffset offsets unix =
    let
        new =
            fromTime (unix + offsetToMS offsets)
    in
    { new | offset = offsets }
