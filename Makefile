
.PHONY: all stats test_all
DIRS = $(shell find * -type d -prune | fgrep -v verify)

all: 
	for i in $(DIRS); do echo $$i; (cd $$i && make); done

stats: 
	for i in $(DIRS); do echo $$i; (cd $$i && make stats); done

test_all: 
	for i in $(DIRS); do echo "\nRunning tests for $$i\n"; (cd $$i && make test); done

clean: 
	for i in $(DIRS); do (cd $$i && make clean); done
