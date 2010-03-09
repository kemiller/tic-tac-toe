
.PHONY: clean test stats

ttt:	TicTacToe.cobra
	cobra -test -target:exe -out:ttt TicTacToe.cobra

clean: 
	rm -f ttt

test:	ttt
	ruby ../verify/ttt_test.rb

stats:
	wc TicTacToe.cobra 
