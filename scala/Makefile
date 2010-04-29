
.PHONY: clean test stats ttt

ttt:	TicTacToe.scala
	scalac TicTacToe.scala

clean: 
	rm -f *.class

test:	ttt
	ruby ../verify/ttt_test.rb

stats:	
	wc TicTacToe.scala
