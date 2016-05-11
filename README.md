# elm-iso8601

A pure Elm library for ISO 8601 time parsing and handling.

See [Elm Packages](http://package.elm-lang.org/packages/jweir/elm-iso8601/latest) for the documentation.

![XKCD ISO8602](http://imgs.xkcd.com/comics/iso_8601.png)

[image by XKCD](http://xkcd.com/1179/)

This package provides functionality for working with time and strings based
on the ISO 8601 standard i.e. `2016-03-31T12:13:14.22-04:00`

It is a "pure" Elm package - no Native bindings to Javascript's Date are used.
This does have a performance cost. This package is about 10x slower than the
core Date library. But there are advantages:

* _Does not cast the time in the local timezone_
* Preserves the timezone offset values
* Detects invalid dates
* Provides a record with easy access to the time's components
* Specific error messages

### TODO
* Get the day of the week
* Support for simple math (Add, Subtract and Durations)
* Support the ISO 8601 range syntax

If you have worked with time in Javascript you may have come
across issues where the time is recast in the local timezone – not fun.

For example with Elm.Date (which uses Javascript)

````elm
Date.fromString \
  "2016-01-01T01:30:00-04:00" \
  |> Result.map (\d -> Date.year d)
--Ok 2015 : Result.Result String Int
````

While the above is the correct time, it is looses its context – year is 2015!

Now with ISO8601
````elm
import ISO8601

t = ISO8601.fromString "2016-01-01T01:30:00-04:00"
-- Ok { year = 2016, month = 1, day = 1, hour = 1, minute = 30, second = 0, millisecond = 0, offset = (-4,0) }
    : Result.Result String ISO8601.Time

t |> Result.map .year
-- Ok 2016 : Result.Result String Int

````

ISO8601 strives to offer better error handling:
````elm
import Date
import ISO8601

-- The below is not a valid date...
Date.fromString "2014-02-30"
-- Ok {} : Result.Result String Date.Date

ISO8601.fromString "2014-02-30"
--- Err ("day is out of range") : Result.Result String ISO8601.Time
````

Error messages specify the first error found:
````elm

Date.fromString "2014-04-02T13:01:61"
-- Err ("unable to parse '2014-04-02T13:01:61' as a date")

ISO8601.fromString "2014-04-02T13:01:61"
-- Err ("second is out of range") : Result.Result String ISO8601.Time
````

