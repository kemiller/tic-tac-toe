
.PHONY: ttt clean test

ttt:	tic-tac-toe.ss	
	mzc --exe ttt tic-tac-toe.ss

clean: 
	rm -f ttt 

test:	ttt
	ruby ../verify/ttt_test.rb
