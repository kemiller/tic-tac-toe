
.PHONY: ttt clean test

ttt:	ticTacToe.ml main.ml
	ocamlopt -o ttt ticTacToe.ml main.ml

clean: 
	rm -f *.o *.cmx *.cmi ttt

test:	ttt
	ruby ../verify/ttt_test.rb
