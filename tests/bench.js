var count = 5000000
var times = new Array(count).map(function(n){
  return new Date(n * count)
}).map(function(d){ return d.getTime() })

console.log(times.length)
