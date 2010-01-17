
.PHONY: ttt clean test stats

ttt:	ttt.cbl
	cobc -x ttt.cbl

clean: 
	rm -f ttt 

test:	ttt
	ruby ../verify/ttt_test.rb

stats:
	wc ttt.cbl
