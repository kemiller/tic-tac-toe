
.PHONY: ttt clean test

ttt:	
	ocamlopt -o ttt ticTacToe.ml main.ml

clean: 
	rm -f *.o *.cmx *.cmi ttt

test: 
	ruby ../verify/ttt_test.rb
