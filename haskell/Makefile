
.PHONY: ttt clean test

ttt:	ticTacToe.hs
	ghc -o ttt TicTacToe.hs

clean: 
	rm -f *.o *.hi ttt

test:	ttt
	ruby ../verify/ttt_test.rb
