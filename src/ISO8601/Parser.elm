module ISO8601.Parser exposing
    ( ComponentTime
    , offset
    , run
    )

import Char
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , andThen
        , end
        , float
        , int
        , keyword
        , map
        , oneOf
        , succeed
        , symbol
        )


type alias ComponentTime =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    , minute : Int
    , second : Float
    , offset : Offset
    }


type alias Offset =
    ( Int, Int )


parse : Parser ComponentTime
parse =
    succeed ComponentTime
        |= paddedInt 4
        |= monthOrDay
        |= monthOrDay
        |= hour
        |= minute
        |= seconds
        |= offset


monthOrDay : Parser Int
monthOrDay =
    oneOf
        [ succeed identity
            |. symbol "-"
            |= paddedInt 2
        , succeed 1
        ]


hour : Parser Int
hour =
    oneOf
        [ succeed identity
            |. symbol "T"
            |= paddedInt 2
        , succeed 0
        ]


minute : Parser Int
minute =
    oneOf
        [ succeed identity
            |. symbol ":"
            |= paddedInt 2
        , succeed 0
        ]


seconds : Parser Float
seconds =
    oneOf
        [ succeed
            (\secs ms ->
                (String.fromInt secs ++ "." ++ ms)
                    |> String.toFloat
                    |> Maybe.withDefault 0.0
            )
            |. symbol ":"
            |= paddedInt 2
            |= oneOf
                [ succeed identity
                    |. oneOf [ symbol ".", symbol "," ]
                    |= manyInts
                , succeed "0"
                ]
        , succeed 0
        ]


offset : Parser Offset
offset =
    let
        makeOffset mod int =
            let
                h =
                    int // 100

                m =
                    int |> modBy 100
            in
            ( mod * h, m )

        sym =
            oneOf
                [ succeed 1
                    |. symbol "+"
                , succeed -1
                    |. symbol "-"
                ]
    in
    Parser.oneOf
        [ succeed makeOffset
            |= sym
            |= chompOffset
        , succeed ( 0, 0 )
            |. oneOf [ symbol "Z", Parser.end ]
        ]


chompOffset : Parser Int
chompOffset =
    Parser.chompWhile (\c -> Char.isDigit c || c == ':')
        |> Parser.getChompedString
        |> Parser.andThen
            (\str ->
                let
                    cleaned =
                        String.replace ":" "" str
                in
                if String.length cleaned == 4 then
                    case String.toInt cleaned of
                        Just val ->
                            Parser.succeed val

                        Nothing ->
                            Parser.problem ("Invalid integer: \"" ++ cleaned ++ "\"")

                else
                    Parser.problem
                        ("Expected "
                            ++ String.fromInt 4
                            ++ " digits, but got "
                            ++ String.fromInt (String.length cleaned)
                        )
            )


{-| A fixed-length integer padded with zeroes.
-}
paddedInt : Int -> Parser Int
paddedInt quantity =
    Parser.chompWhile Char.isDigit
        |> Parser.getChompedString
        |> Parser.andThen
            (\str ->
                if String.length str == quantity then
                    -- StringtoInt works on zero-padded integers
                    case String.toInt str of
                        Just intVal ->
                            Parser.succeed intVal

                        Nothing ->
                            Parser.problem ("Invalid integer: \"" ++ str ++ "\"")

                else
                    Parser.problem
                        ("Expected "
                            ++ String.fromInt quantity
                            ++ " digits, but got "
                            ++ String.fromInt (String.length str)
                        )
            )


manyInts : Parser String
manyInts =
    Parser.chompWhile Char.isDigit
        |> Parser.getChompedString


run : String -> Result (List Parser.DeadEnd) ComponentTime
run s =
    Parser.run parse s
