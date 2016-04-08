module Bench where

import ISO8601 
import Debug
import ElmTest exposing(..)
import Array

count = 500000

time n =
  let
      x = ISO8601.fromTime n |> ISO8601.toString |> ISO8601.fromString
  in
    case n < count of
      True ->
        time (n + 1)
      False ->
        n

all = 
  let
    x = time 0
  in
    -- suite "benchmark" (Array.toList times)
    suite "benchmark" [ x `equals` count ]
