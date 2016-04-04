.PHONY: test
test:
	elm-test TestRunner.elm

bench: 
	ruby make_dates.rb
	b=`time elm-test BenchRunner.elm`
	a=`time node bench.js`


