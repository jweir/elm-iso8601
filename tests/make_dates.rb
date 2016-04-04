require 'set'
require 'time'

count = 10000
stringTimes = Set.new

base = 1459795934
while stringTimes.size < count do
  t = Time.at( base * 2 - ((base * 6) * rand))
  stringTimes << t.iso8601
end

temp = <<-EOF
module Bench where

import ISO8601 
import Debug
import ElmTest exposing(..)

times = 
  [
    #{stringTimes.map {|s| "\"#{s}\"" }.join "\n  ,"}
  ]

all = 
  suite "benchmark"
    [
    (List.length (List.map (\\ d -> ISO8601.parse d |> ISO8601.toTime) times)) `equals` #{count}
    ]
EOF

File.write 'Bench.elm', temp


js_temp = <<-EOF
var times = [
  #{stringTimes.map {|s| "\"#{s}\"" }.join "\n  ,"}
]

console.log(times.map(function(t){ return new Date(Date.parse(t)).toISOString()}).length)
// console.log(times)
EOF

File.write 'bench.js', js_temp

