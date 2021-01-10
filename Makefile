#
# Copyright (C) 2020-2021 Sébastien Helleu <flashcode@flashtux.org>
#
# Postman-to-emacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Postman-to-emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Postman-to-emacs.  If not, see <https://www.gnu.org/licenses/>.
#

EMACS ?= emacs
RM ?= rm -f

.PHONY: all test compile clean

all: compile test

test: clean
	$(EMACS) -Q -batch -L . -l postman -l postman-tests -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q -batch -f batch-byte-compile postman.el

clean:
	$(RM) *.elc
