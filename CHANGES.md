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
