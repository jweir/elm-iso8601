module ISO8601.Helpers (isLeapYear) where

isLeapYear : Int -> Bool
isLeapYear year =
  let
    -- A If the year is evenly divisible by 4, go to step B
    a =
      year % 4

    -- B If the year is evenly divisible by 100, go to step C
    b =
      year % 100 % 2

    -- C If the year is evenly divisible by 400, go to step D
    c =
      year % 400 % 2
  in
    case [ a, b, c ] of
      [ 0, 0, 0 ] ->
        True

      _ ->
        False