.PHONY: all clean compile test

EMACS ?= emacs

SRCS = org-invoice.el org-invoice-export.el

all: compile

compile:
	$(EMACS) -Q -batch -L . \
		-f batch-byte-compile $(SRCS)

clean:
	rm -f *.elc

test:
	$(EMACS) -Q -batch -L . \
		-l org-invoice.el \
		-l org-invoice-export.el \
		-l test/org-invoice-test.el \
		-f ert-run-tests-batch-and-exit
