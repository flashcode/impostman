#
# Copyright (C) 2020-2021 Sébastien Helleu <flashcode@flashtux.org>
#
# Impostman is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Impostman is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Impostman.  If not, see <https://www.gnu.org/licenses/>.
#

EMACS ?= emacs
RM ?= rm -f

.PHONY: all test compile clean

all: compile test

test: clean
	$(EMACS) -Q -batch -L . -l impostman -l tests/impostman-tests -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q -batch -f batch-byte-compile impostman.el

clean:
	$(RM) *.elc
