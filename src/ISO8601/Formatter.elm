module ISO8601.Formatter exposing (toString)

import String exposing (padLeft)
import ISO8601.Types exposing (Time, Offset)


{-| type of formatter component functions
-}
type alias Fmt =
    ( String, Time ) -> ( String, Time )


{-| normalize api from formatter components
-}
fmt : Fmt -> Time -> String
fmt f time =
    let
        ( str, _ ) =
            f ( "", time )
    in
        str


{-| append year to the formatted string
-}
year : Fmt
year ( str, time ) =
    ( str ++ (Basics.toString time.year), time )


{-| append month to the formatted string
-}
month : Fmt
month ( str, time ) =
    ( str ++ (Basics.toString time.month), time )


{-| append day to the formatted string
-}
day : Fmt
day ( str, time ) =
    ( str ++ (Basics.toString time.day), time )


{-| append hour to the formatted string
-}
hour : Fmt
hour ( str, time ) =
    ( str ++ (Basics.toString time.hour), time )


{-| append minute to the formatted string
-}
minute : Fmt
minute ( str, time ) =
    ( str ++ (Basics.toString time.minute), time )


{-| append second to the formatted string
-}
second : Fmt
second ( str, time ) =
    ( str ++ (Basics.toString time.second), time )


{-| append millisecond to the formatted string
-}
millisecond : Fmt
millisecond ( str, time ) =
    let
        ms =
            if time.millisecond == 0 then
                ""
            else
                "." ++ (padLeft 3 '0' (Basics.toString time.millisecond))
    in
        ( str ++ ms, time )


{-| append offset to the formatted string (eg "Z", "+0200")
-}
offset : Fmt
offset ( str, time ) =
    case time.offset of
        ( 0, 0 ) ->
            ( str ++ "Z", time )

        ( h, m ) ->
            let
                symbol =
                    if h >= 0 then
                        "+"
                    else
                        "-"

                pad x =
                    padLeft 2 '0' (Basics.toString x)

                ofs =
                    symbol ++ (pad (abs h)) ++ (pad m)
            in
                ( str ++ ofs, time )


{-| append symbol/string to the formatted string
-}
sym : String -> Fmt
sym s ( str, time ) =
    ( str ++ s, time )


{-| pad formatter component output with zeros
-}
pad0 : Int -> Fmt -> Fmt
pad0 p f ( str, time ) =
    let
        ( s, _ ) =
            f ( "", time )
    in
        ( str ++ (padLeft p '0' s), time )


{-| Converts a Time record to an ISO 8601 formated string.
-}
toString : Time -> String
toString =
    fmt
        (pad0 4 year
            >> sym "-"
            >> pad0 2 month
            >> sym "-"
            >> pad0 2 day
            >> sym "T"
            >> pad0 2 hour
            >> sym ":"
            >> pad0 2 minute
            >> sym ":"
            >> pad0 2 second
            >> millisecond
            >> offset
        )
