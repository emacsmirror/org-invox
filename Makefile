.PHONY: all clean compile test

EMACS ?= emacs

SRCS = org-invox.el org-invox-export.el

all: compile

compile:
	$(EMACS) -Q -batch -L . \
		-f batch-byte-compile $(SRCS)

clean:
	rm -f *.elc

test:
	$(EMACS) -Q -batch -L . \
		-l org-invox.el \
		-l org-invox-export.el \
		-l test/org-invox-test.el \
		-f ert-run-tests-batch-and-exit
