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

OCAML-PROGRAMS    := lsd-mount
OCAML-MODULES     := trace subprocess io_task shell_procedure mountlib linux
LAMA-MODULES      := trace subprocess io_task shell_procedure
OCAML-LIBS        := unix str


OCAMLC            := ocamlc
OCAMLMKTOP        := ocamlmktop

OCAMLOPT          := ocamlopt
OCAMLOPT-FLAGS    := $(OCAML-FLAGS) -compact 

STRIP             := strip

INSTALL           := install
DEST              := 
INSTALL-MODE      := -m 755
INSTALL-MODE-SUID := -m 4755 -o root -g root 

# -----------------------------------------------------------------------------------

include ModuleDeps.mk

$(LAMA-MODULES:%=%.ml) $(LAMA-MODULES:%=%.mli) : %: lama/%
	cp $< $@

# -----------------------------------------------------------------------------------

ifneq ($(USER),root)
                   INSTALL-MODE-SUID := $(INSTALL-MODE)
	           DEST              := $(shell pwd)/FILES
endif

FAIL = { rm -f "$@" ; exit 1 ; }

# -----------------------------------------------------------------------------------

.PRECIOUS += %.cmi

%.cmi: %.mli
	$(OCAMLOPT) $(OCAMLOPT-FLAGS) -c $< || $(FAIL)

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPT-FLAGS) -c $< || $(FAIL)

%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAML-FLAGS) -c $< || $(FAIL)

$(OCAML-PROGRAMS): %: %.ml $(OCAML-MODULES:%=%.cmx)
	$(OCAMLOPT) $(OCAML-LIBS:%=%.cmxa)  $(filter %.cmx,$^) $(OCAMLOPT-FLAGS) -o $@ $<  || $(FAIL)
	$(STRIP) $@

%.top: $(OCAML-MODULES:%=%.cmo)
	$(OCAMLMKTOP) -o $@ $(OCAML-LIBS:%=%.cma) $(filter %.cmo,$^) 

# -----------------------------------------------------------------------------------

.PRECIOUS: $(.PRECIOUS)

clean:
	rm -f *.cmx *.cmi *.cma *.cmo *.o *~ a.out $(OCAML-PROGRAMS) PATCHES/*~ *.top 
	rm -f $(LAMA-MODULES:%=%.ml) $(LAMA-MODULES:%=%.mli)
	rm -rf FILES


all: $(OCAML-PROGRAMS)

install: all
	$(INSTALL) -d $(DEST)/sbin
	# $(INSTALL) $(INSTALL-MODE-SUID) $(filter %mount.lcrypt,$(OCAML-PROGRAMS)) $(DEST)/sbin
	$(INSTALL) $(INSTALL-MODE-SUID) lsd-mount $(DEST)/sbin/lsd-mount.suid
	for i in mount.lcrypt umount.lcrypt; do ln -f $(DEST)/sbin/lsd-mount.suid $(DEST)/sbin/$$i; done
	# $(INSTALL) $(INSTALL-MODE) $(filter-out %mount.lcrypt,$(OCAML-PROGRAMS)) $(DEST)/sbin
	$(INSTALL) $(INSTALL-MODE) lsd-mount $(DEST)/sbin/lsd-mount.bin
	for i in loclean mkfs.lcrypt fsck.lcrypt; do ln -f $(DEST)/sbin/lsd-mount.bin $(DEST)/sbin/$$i; done

uninstall: 
	rm -f $(OCAML-PROGRAMS:%=$(DEST)/sbin/%)
	rmdir $(DEST)/sbin || true

deps:
	ocamldep $(OCAML-MODULES:%=%.ml) $(OCAML-PROGRAMS:%=%.ml) > ModuleDeps.mk

top: lcrypt.top


test:

