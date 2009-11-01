
.PHONY: ttt clean test

ttt:	tic_tac_toe.c	
	gcc -ansi -pedantic -o ttt tic_tac_toe.c

clean: 
	rm -f ttt 

test:	ttt
	ruby ../verify/ttt_test.rb
