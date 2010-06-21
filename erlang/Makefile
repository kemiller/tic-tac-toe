
.PHONY: clean test stats ttt

ttt:	tic_tac_toe.erl
	erlc tic_tac_toe.erl

clean: 
	rm -f *.beam

test:	ttt
	ruby ../verify/ttt_test.rb

stats:	
	wc tic_tac_toe.erl
