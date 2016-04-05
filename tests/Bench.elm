module Bench where

import ISO8601 
import Debug
import ElmTest exposing(..)
import Array

base = 1459795934
count = 5000000

-- times = 
  -- Array.initialize (count) (\n -> ISO8601.fromTime(n * count) |> ISO8601.toTime |> equals (n * count ))

time n =
  let
      x = ISO8601.fromTime n |> ISO8601.toTime
  in
    case n < count of
      True ->
        time (n + 1)
      False ->
        n

all = 
  let
    _ = time 0
  in
    -- suite "benchmark" (Array.toList times)
    suite "benchmark" [ 1 `equals` 1 ]
