var count = 500000

var times = []
function time(){
  for(var n =0; n < count; n++){
    var a = (new Date(n)).toISOString()
    times.push(new Date(Date.parse(a)).getTime())
  }
}

time(0)
console.log("done", count, times.length)
