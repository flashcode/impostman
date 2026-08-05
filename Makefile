# SPDX-FileCopyrightText: 2020-2026 Sébastien Helleu <flashcode@flashtux.org>
#
# SPDX-License-Identifier: GPL-3.0-or-later

EMACS ?= emacs
RM ?= rm -f

.PHONY: all compile test clean

all: compile test

compile:
	$(EMACS) -Q -batch --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile impostman.el

test: clean
	cd tests && $(EMACS) -Q -batch -L . -L .. -l impostman -l impostman-test -f ert-run-tests-batch-and-exit

clean:
	$(RM) *.elc
