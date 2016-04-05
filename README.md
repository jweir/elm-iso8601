# elm-iso8601

![XKCD ISO8602](http://imgs.xkcd.com/comics/iso_8601.png)

[image by XKCD](http://xkcd.com/1179/)

This package provides functionality for working with time and strings based
on the ISO 8601 standard i.e. `2016-03-31T12:13:14.22-04:00`

It is a "pure" Elm package – no Native bindings to Javascript's Date are used.
This does have a performance cost. This package is about 10x slower than the
native Date library. But there are advantages:

* _Does not cast the time in the local timezone_
* Preserves the timezone offset values.
* Always returns a time. Invalid input results in a time of `000-01-01T00:00:00Z`

If you have worked with timestamps in a browser you may have come
across issues where the time is recast in the local timezone.

For example in Javascript

````javascript
new Date(Date.parse("2016-01-01T01:30:00-04:00"))
// Thu Dec 31 2015 21:30:00 GMT-0800 (PST)
````

While the above is the correct time, it is looses its context – if you grab the year it is 2015!

Now using ISO8601
````elm
import ISO8601

t = ISO8601.fromString "2016-01-01T01:30:00-04:00"
-- { year = 2016, month = 1, day = 1, hour = 1, minute = 30, second = 0, millisecond = 0, offset = (-4,0) }
    : ISO8601.Time

ISO8601.toString t
-- "2016-01-01T01:30:00-0400" : String

````

Example of compatibility with `Elm.Date`
````elm

import ISO8601
import Date

i = ISO8601.fromString "2016-01-01T01:30:00-04:00" |> ISO8601.toTime
-- 1451626200000 : ISO8601.Millisecond

d = i |> toFloat |> Date.fromTime
-- {} : Date.Date
Date.year d
-- 2015 : Int
-- uh, back to our Javascript time casting problem
````

TODO
* Get the day of the week
* Support for simple math (Add, Subtract and Durations)
* Support the ISO 8601 range syntax

