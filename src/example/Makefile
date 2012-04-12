# $Id
# Example of use of DBForge

include ../../master.Makefile

COMPFLAGS=
SRCFILES= \
	base.ml \
	example.ml

all: example.byte

example.byte: $(SRCFILES)
	$(OCAMLFIND) ocamlc -o $@ -package str,dbforge.mysql,unix -linkpkg  $(SRCFILES)

base.ml: base.xml
	$(ROOT)/src/dbforge -o base.ml base.xml

clean:
	$(RM) *.cm* *~ base.ml example.byte
