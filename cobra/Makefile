
.PHONY: clean test stats

ttt:	TicTacToe.cobra
	cobra -c -target:exe TicTacToe.cobra

clean: 
	rm -f *.cs *.mdb *.exe

test:	TicTacToe.exe
	ruby ../verify/ttt_test.rb

stats:
	wc TicTacToe.cobra 
