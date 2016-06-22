emacs ?= emacs

all: test

test:
	$(emacs) -batch -l targets/compile.el -l pamparam.el -l pam-test.el -f ert-run-tests-batch-and-exit

compile:
	$(emacs) -batch -l targets/compile.el

clean:
	rm -f *.elc

.PHONY: all compile clean test
