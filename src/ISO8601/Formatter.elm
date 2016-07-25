module ISO8601.Formatter exposing (toString)

import String
import ISO8601.Types exposing(Time, Offset)

fmtMs : Int -> String
fmtMs n =
    if n == 0 then
        ""
    else 
      "." ++ (pad0 3 n)

{-| pad left zeros -}
pad0 : Int -> Int -> String
pad0 size n = String.padLeft size '0' (Basics.toString n)

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
                symbol ++ (pad0 2 (abs h)) ++ (pad0 2 m)


{-| Converts a Time record to an ISO 8601 formated string.
-}
toString : Time -> String
toString time =
    String.join ""
        [ pad0 4 time.year
        , "-"
        , pad0 2 time.month
        , "-"
        , pad0 2 time.day
        , "T"
        , pad0 2 time.hour
        , ":"
        , pad0 2 time.minute
        , ":"
        , pad0 2 time.second
        , fmtMs time.millisecond
        , fmtOffset time.offset
        ]
