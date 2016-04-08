var count = 500000

function time(){
  for(var n =0; n < count; n++){
    var a = (new Date(n)).toISOString()
    new Date(Date.parse(a))
  }
}

time(0)
console.log("done", count)
