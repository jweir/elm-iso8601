# 8.0.0

## Breaking Change
The library now strict about parsing times.  Before it was liberal in accepting strings with dates or times in them.
Now only a valid ISO8601 date/time string will be parsed.


# 7.0.0
Breaking change - offset changed from a tuple of (hour, minute) to an Int of minutes.
This fixes a bug found by @jinjor for when an offset is less than one hour and
negative. Is there such an offset? Not that we can find.

# 6.0.1
fix fromTime incorrecly rounding milliseconds up to a second - @tom2

# 6.0.0
Add a JSON decoder
Use the Time.Weekday type

# 5.0.2
Fix large ints.  The seconds value was not getting correctly handled.

# 5.0.1
Create an error when a string is empty - thanks @girishso
Changed error message from "unknown error" to "Unable to parse time"


# 5.0.0
Back to Int. Since the `elm/time` `Posix` is integer based.

* Adds toPosix and fromPosix to be compatible with `elm/time`

# 4.0.1
* Use Float for toTime and fromTime to be compatible with core Time.Time
* Created diff, sub, and add functions

Note version 4.0.0 was releases but the pacakge had the incorrect version number. Thanks @jesse-c

# 3.0.1
* Fix an infinite loop when handling New Years

# 3.0.0
* Hide the time model internals

# 2.1.1
* Fix rounding error at the boundary of months

# 2.1.0
* Converted to Elm 0.17

# 2.0.0
* fromString returns a Result String Time

# 1.0.0
* Released
