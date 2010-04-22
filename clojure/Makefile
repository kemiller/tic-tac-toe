
.PHONY: clean test stats

ttt:	tic-tac-toe.clj	
	mzc --exe ttt tic-tac-toe.ss

clean: 

test:	ttt
	ruby ../verify/ttt_test.rb

stats:
	wc tic-tac-toe.clj
