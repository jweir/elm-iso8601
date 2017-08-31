module ISO8601.Math exposing (diff, sub)

import ISO8601 exposing (Time, toTime, Offset)


diff : Time -> Time -> Float
diff a b =
    (toTime a) - (toTime b)


sub : Time -> Float -> Time
sub time amount =
    (toTime time) - amount |> fromTimeWithOffset time.offset


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
