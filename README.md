# elm-iso8601

![XKCD ISO8602](http://imgs.xkcd.com/comics/iso_8601.png)

[image by XKCD](http://xkcd.com/1179/)

elm-8601 is an Elm native library to parse iso8601 formated strings, convert them to Unix time and vice-versa. 

Since it does not use Javascript `Date` function, timezones and offsets are not altered by the browser.

The code has not been optimized and is surely slower than native Javascript parsing.

TODO
* support fractional time parsing
* decide to support miliseconds and Javascript compatible UNIX time or something else (seconds, milliseconds) ?
* Add documentation
* toString 

NON-GOALS
* String formating - the ISO8601 record is easy to create a formatter from