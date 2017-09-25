module ISO8601.Math exposing (add, diff, sub)

{-| This package provides functionality for working with time and strings based
on the ISO 8601 standard i.e. `2016-03-31T12:13:14.22-04:00`


# Time record

@docs Time, Weekday, Offset


# Accessors

@docs year, month, day, hour, minute, second, millisecond, offset, weekday


# Parsing

@docs fromString, toString


# Time conversion

@docs toTime, fromTime

-}

import ISO8601 exposing (Offset, Time, toTime)


diff : Time -> Time -> Float
diff a b =
    toTime a - toTime b


sub : Time -> Float -> Time
sub time amount =
    toTime time - amount |> fromTimeWithOffset time.offset


add : Time -> Float -> Time
add time amount =
    toTime time + amount |> fromTimeWithOffset time.offset


offsetToMS : Offset -> Float
offsetToMS offset =
    let
        ( hour, minutes ) =
            offset
    in
    (hour * 60 * 60 * 1000) + (minutes * 60 * 1000) |> toFloat


fromTimeWithOffset : Offset -> Float -> Time
fromTimeWithOffset offset unix =
    let
        new =
            ISO8601.fromTime (unix + offsetToMS offset)
    in
    { new | offset = offset }
