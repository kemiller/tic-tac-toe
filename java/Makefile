
.PHONY: clean test stats ttt

ttt:	TicTacToe.java
	javac TicTacToe.java

clean: 
	rm -f *.class

test:	ttt
	ruby ../verify/ttt_test.rb

stats:	
	wc TicTacToe.java
