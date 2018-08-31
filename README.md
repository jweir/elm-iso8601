# elm-iso8601

A Elm library for parsing ISO 8601 time strings, preserving the time offset.

See [Elm Packages](http://package.elm-lang.org/packages/jweir/elm-iso8601/latest) for the documentation.

This package provides functionality for working with time and strings based
on the ISO 8601 standard i.e. `2016-03-31T12:13:14.22-04:00`

* _Does not cast the time in the local timezone._
* Preserves the timezone offset values.
* Provides a record with access to the local time's components.
  Useful if your app needs to group large number of records by day, hour, or month.


````elm
import ISO8601

t = ISO8601.fromString "2016-01-01T01:30:00-04:00"
-- Ok { year = 2016, month = 1, day = 1, hour = 1, minute = 30, second = 0, millisecond = 0, offset = (-4,0) }
    : Result.Result String ISO8601.Time

````

ISO8601 strives to offer specifc error handling:
````elm
import ISO8601


ISO8601.fromString "2014-02-30"
--- Err ("day is out of range") : Result.Result String ISO8601.Time

Date.fromString "2014-04-02T13:01:61"
-- Err ("unable to parse '2014-04-02T13:01:61' as a date")

ISO8601.fromString "2014-04-02T13:01:61"
-- Err ("second is out of range") : Result.Result String ISO8601.Time
````

