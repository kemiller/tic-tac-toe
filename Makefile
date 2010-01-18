
.PHONY: all stats test_all
DIRS = $(shell find * -type d -prune | fgrep -v verify)

test: 
	for i in $(DIRS); do echo "\nRunning tests for $$i\n"; (cd $$i && make test); done

stats: 
	for i in $(DIRS); do echo $$i; (cd $$i && make stats); done

clean: 
	for i in $(DIRS); do (cd $$i && make clean); done
