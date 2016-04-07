module ISO8601.Helpers (EpochRelative (..), daysToYears, daysToMonths, isLeapYear, daysInYear, daysInMonth, toInt) where

import Array
import String

toInt : String -> Int
toInt str =
  String.toInt str 
    |> Result.toMaybe 
    |> Maybe.withDefault(0)

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


daysInMonth : Int -> Int -> Int
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


daysInYear : Int -> Int
daysInYear year =
  if isLeapYear (year) then
    366
  else
    365

type EpochRelative = Before | After

-- from a starting year returns the ending year and remaing days
daysToYears : EpochRelative -> Int -> Int -> ( Int, Int )
daysToYears rel startYear remainingDays =
  case rel of
    After ->
      let
        remainingDays' =
          remainingDays - daysInYear startYear
      in
        if remainingDays' > 0 then
          daysToYears After (startYear + 1) remainingDays'
        else
          ( startYear, remainingDays )
    Before ->
      let
        remainingDays' =
          remainingDays + daysInYear startYear
      in
        if remainingDays' < 0 then
          daysToYears Before (startYear - 1) remainingDays'
        else
          ( startYear,  (daysInYear startYear) + remainingDays )


-- remaingDays will alawys be less than 366


daysToMonths : Int -> Int -> Int -> ( Int, Int )
daysToMonths year startMonth remainingDays =
  let
    remainingDays' =
      remainingDays - daysInMonth year startMonth
  in
    if remainingDays' > 0 then
      daysToMonths year (startMonth + 1) remainingDays'
    else
      ( startMonth, remainingDays )

