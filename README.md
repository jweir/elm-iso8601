# elm-iso8601

ISO 8601 time parsing and conversion for Elm.

See [Elm Packages](http://package.elm-lang.org/packages/jweir/elm-iso8601/latest) for the documentation.

![XKCD ISO8602](http://imgs.xkcd.com/comics/iso_8601.png)

[image by XKCD](http://xkcd.com/1179/)

This package provides functionality for working with time and strings based
on the ISO 8601 standard i.e. `2016-03-31T12:13:14.22-04:00`

It is a "pure" Elm package – no Native bindings to Javascript's Date are used.
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

