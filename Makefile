#
#   lsd-mount-utilities -- Utilities to mount encrypted filesystems from /etc/fstab
#
#   Copyright (C) 2006,2007  M E Leypold, Software-Dienstleistungen und Beratung
#
#   -------------------------------------------------------------------------------
#
#   This program is free software; you can redistribute it and/or
#   modify it under the terms of the GNU General Public License
#   version 2 as published by the Free Software Foundation (no later
#   version).
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#   General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
#   02110-1301 USA.
#
#   -------------------------------------------------------------------------------
#
#   Makefile -- This is a GNU-Makefile for the lsd-mount-utilities
#


default: all

OCAML-PROGRAMS    := mount.lcrypt loclean umount.lcrypt
OCAML-MODULES     := subprocess mountlib
OCAML-LIBS        := unix str

OCAMLOPT          := ocamlopt
OCAMLOPT-FLAGS    := -compact

STRIP             := strip

INSTALL           := install
DEST              := 
INSTALL-MODE      := -m 755
INSTALL-MODE-SUID := -m 4755 -o root -g root 

# -----------------------------------------------------------------------------------

.PRECIOUS: %.cmi

ifneq ($(USER),root)
                   INSTALL-MODE-SUID := $(INSTALL-MODE)
	           DEST              := $(shell pwd)/FILES
endif

FAIL = { rm -f "$@" ; exit 1 ; }


%.cmi: %.mli
	$(OCAMLOPT) $(OCAMLOPT-FLAGS) -c $< || $(FAIL)

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPT-FLAGS) -c $< || $(FAIL)

$(OCAML-PROGRAMS): %: %.ml $(OCAML-MODULES:%=%.cmx)
	$(OCAMLOPT) $(OCAML-LIBS:%=%.cmxa)  $(filter %.cmx,$^) $(OCAMLOPT-FLAGS) -o $@ $<  || $(FAIL)
	$(STRIP) $@

clean:
	rm -f *.cmx *.cmi *.o *~ a.out $(OCAML-PROGRAMS) PATCHES/*~
	rm -rf FILES


all: $(OCAML-PROGRAMS)

install: all
	$(INSTALL) -d $(DEST)/sbin
	$(INSTALL) $(INSTALL-MODE-SUID) $(filter %.lcrypt,$(OCAML-PROGRAMS)) $(DEST)/sbin
	$(INSTALL) $(INSTALL-MODE) $(filter-out %.lcrypt,$(OCAML-PROGRAMS)) $(DEST)/sbin

uninstall: 
	rm -f $(OCAML-PROGRAMS:%=$(DEST)/sbin/%)
	rmdir $(DEST)/sbin || true
test:
