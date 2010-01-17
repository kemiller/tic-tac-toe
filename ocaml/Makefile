
.PHONY: ttt clean test stats

ttt:	ticTacToe.ml
	ocamlopt -o ttt ticTacToe.ml

clean: 
	rm -f *.o *.cmx *.cmi ttt

test:	ttt
	ruby ../verify/ttt_test.rb

stats:
	wc ticTacToe.ml 
