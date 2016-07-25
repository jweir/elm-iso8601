module ISO8601.Formatter exposing (toString)

import String
import ISO8601.Types exposing(Time, Offset)

fmt : Int -> String
fmt n =
    if n < 10 then
        "0" ++ Basics.toString n
    else
        Basics.toString n


fmtYear : Int -> String
fmtYear n =
    let
        s =
            Basics.toString n
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
        ".00" ++ Basics.toString n
    else if n < 100 then
        ".0" ++ Basics.toString n
    else
        "." ++ Basics.toString n

fmtOffset : Offset -> String
fmtOffset offset =
    case offset of
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
                symbol ++ (fmt (abs h)) ++ (fmt m)


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

