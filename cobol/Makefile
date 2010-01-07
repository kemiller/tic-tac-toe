
.PHONY: ttt clean test

ttt:	ttt.cbl
	cobc -x ttt.cbl

clean: 
	rm -f ttt 

test:	ttt
	ruby ../verify/ttt_test.rb
