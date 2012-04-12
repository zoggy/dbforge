#################################################################################
#                Caml-get                                                       #
#                                                                               #
#    Copyright (C) 2003-2012 Institut National de Recherche en Informatique     #
#    et en Automatique. All rights reserved.                                    #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU Lesser General Public License version        #
#    3 as published by the Free Software Foundation.                            #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
#    GNU General Public License for more details.                               #
#                                                                               #
#    You should have received a copy of the GNU General Public License          #
#    along with this program; if not, write to the Free Software                #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#                                                                               #
#################################################################################

include ../master.Makefile

PACKAGES=str,config-file,diff
GTK_PACKAGES=lablgtk2-extras.configwin,diff.gtk
ALL_PACKAGES=$(PACKAGES),$(GTK_PACKAGES)
OF_FLAGS=-package $(ALL_PACKAGES)
INCLUDES=-I +ocamldoc

COMPFLAGS=-rectypes $(INCLUDES) -annot
LINKFLAGS=$(INCLUDES)

# The executables to produce
# PLEASE KEEP IN ALPHABETICAL ORDER
EXE=caml-get
EXE_BYTE=$(EXE).byte
EXE_GTK=caml-get.gtk
EXE_GTK_BYTE=$(EXE_GTK).byte

LIB=camlget.cmxa
LIB_BYTE=camlget.cma

LIB_GTK=camlget_gtk.cmxa
LIB_GTK_BYTE=$(LIB_GTK:.cmxa=.cma)

ODOC=odoc_ar.cmxs
ODOC_BYTE=$(ODOC:.cmxs=.cma)
ODOC_HTML=odoc_htmlcg.cmxs
ODOC_HTML_BYTE=$(ODOC_HTML:.cmxs=.cmo)
ODOC_TEST=odoc_cgtest.cmxs
ODOC_TEST_BYTE=$(ODOC_TEST:.cmxs=.cmo)

# Compilation
#############

LIBS_BYTE=odoc_info.cma

LIBS=odoc_info.cmxa

LIB_CMOFILES= \
	cgversion.cmo \
	cgtypes.cmo \
	server_data.cmo \
	client_data.cmo \
	cgmisc.cmo \
	cgnames.cmo \
	camlget.cmo

LIB_CMXFILES= $(LIB_CMOFILES:.cmo=.cmx)
LIB_CMIFILES= $(LIB_CMOFILES:.cmo=.cmi)

LIB_GTK_CMOFILES= camlget_gtk.cmo
LIB_GTK_CMXFILES= $(LIB_GTK_CMOFILES:.cmo=.cmx)
LIB_GTK_CMIFILES= $(LIB_GTK_CMOFILES:.cmo=.cmi)

###
EXE_CMOFILES=main.cmo
EXE_CMXFILES= $(EXE_CMOFILES:.cmo=.cmx)
EXE_CMIFILES= $(EXE_CMOFILES:.cmo=.cmi)

EXE_GTK_CMOFILES=main_gtk.cmo
EXE_GTK_CMXFILES= $(EXE_GTK_CMOFILES:.cmo=.cmx)
EXE_GTK_CMIFILES= $(EXE_GTK_CMOFILES:.cmo=.cmi)

ODOC_CMOFILES=cgtypes.cmo server_data.cmo odoc_ar.cmo

ODOC_CMXFILES= $(ODOC_CMOFILES:.cmo=.cmx)
ODOC_CMIFILES= $(ODOC_CMOFILES:.cmo=.cmi)

all: opt byte
opt: $(LIB) $(EXE) $(LIB_GTK) $(EXE_GTK) $(ODOC) $(ODOC_HTML) $(ODOC_TEST)
byte: $(LIB_BYTE) $(EXE_BYTE) $(LIB_GTK_BYTE) $(EXE_GTK_BYTE) $(ODOC_BYTE) $(ODOC_HTML_BYTE) $(ODOC_TEST_BYTE)

$(LIB): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -a -o $@ $(LIB_CMXFILES)

$(LIB_BYTE): $(LIB_CMIFILES) $(LIB_CMOFILES)
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -a -o $@ $(LIB_CMOFILES)

$(LIB_GTK): $(LIB_GTK_CMIFILES) $(LIB_GTK_CMXFILES)
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -a -o $@ $(LIB_GTK_CMXFILES)

$(LIB_GTK_BYTE): $(LIB_GTK_CMIFILES) $(LIB_GTK_CMOFILES)
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -a -o $@ $(LIB_GTK_CMOFILES)

$(EXE): $(LIB) $(EXE_CMIFILES) $(EXE_CMXFILES)
	$(OCAMLFIND) ocamlopt -package $(PACKAGES) -linkpkg -o $@ $(LINKFLAGS) $(LIBS) $(LIB) $(EXE_CMXFILES)

$(EXE_BYTE): $(LIB_BYTE) $(EXE_CMIFILES) $(EXE_CMOFILES)
	$(OCAMLFIND) ocamlc $ -package $(PACKAGES) -linkpkg -o $@ $(LINKFLAGS) $(LIBS_BYTE) $(LIB_BYTE) $(EXE_CMOFILES)

$(EXE_GTK): $(LIB) $(LIB_GTK) $(EXE_GTK_CMIFILES) $(EXE_GTK_CMXFILES)
	$(OCAMLFIND) ocamlopt -package $(ALL_PACKAGES) -linkpkg -o $@ $(LINKFLAGS) \
	$(LIBS) $(LIB) $(LIB_GTK) $(EXE_GTK_CMXFILES)

$(EXE_GTK_BYTE): $(LIB_BYTE) $(LIB_GTK_BYTE) $(EXE_GTK_CMIFILES) $(EXE_GTK_CMOFILES)
	$(OCAMLFIND) ocamlc -package $(ALL_PACKAGES) -linkpkg -o $@ $(LINKFLAGS) \
	$(LIBS_BYTE) $(LIB_BYTE) $(LIB_GTK_BYTE) $(EXE_GTK_CMOFILES)

$(ODOC): $(ODOC_CMIFILES) $(ODOC_CMXFILES)
	$(OCAMLFIND) ocamlopt -package $(PACKAGES) -linkpkg -shared -o $@ -I +ocamldoc $(ODOC_CMXFILES)

$(ODOC_BYTE): $(ODOC_CMIFILES) $(ODOC_CMOFILES)
	$(OCAMLFIND) ocamlc -package $(PACKAGES) -linkpkg -a -o $@ -I +ocamldoc $(ODOC_CMOFILES)

$(ODOC_HTML): odoc_htmlcg.cmx
	$(OCAMLFIND) ocamlopt -package $(PACKAGES) -linkpkg -shared -o $@ -I +ocamldoc $^

$(ODOC_TEST): odoc_cgtest.cmx
	$(OCAMLFIND) ocamlopt -package $(PACKAGES) -linkpkg -shared -o $@ -I +ocamldoc $^



# Test :
########

test: dummy $(ODOC)
	$(MKDIR) test
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -c test.mli
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -c test.ml
	ocamldoc -v -keep-code -o test/test.cga -I +ocamldoc \
	-hide Cgtypes -prefix "Zoggy." \
	test.mli test.ml -g $(ODOC)

# Documentation :
#################
dump.odoc: *.mli *.ml
	$(OCAMLFIND) ocamldoc $(OF_FLAGS) $(OCAMLPP) $(INCLUDES) -rectypes -sort -dump dump.odoc \
	`ls $^ | grep -v cam_camlget`

doc: dump.odoc
	$(MKDIR) ocamldoc
	$(OCAMLFIND) ocamldoc $(OF_FLAGS) $(OCAMLPP) \
	-load $^ \
	-d ocamldoc -html

doc2: dump.odoc
	$(MKDIR) ocamldoc
	$(OCAMLFIND) ocamldoc $(OF_FLAGS) $(OCAMLPP) \
	-load $^ \
	-d ocamldoc -g $(ODOC_HTML_BYTE)
	$(CP) ../images/camlget.png ocamldoc/

cgtest: dump.odoc dummy
	$(MKDIR) ocamldoc
	$(OCAMLFIND) ocamldoc $(OCAMLPP) -load $< \
	-d ocamldoc -g $(ODOC_TEST_BYTE) -o cgtest.ml-test
	$(OCAMLFIND) ocamlc -package $(PACKAGES) -linkpkg -o $@ $(COMPFLAGS) $(LIBS_BYTE) \
	$(LIB_BYTE) -impl cgtest.ml-test

dot: dep.dot
dep.dot: dump.odoc
	$(OCAMLDOC) -load $< -o $@ -dot -dot-reduce
dep.ps:dep.dot
	dot -Tps -o $@ $<

# backup, clean and depend :
############################

distclean: clean
	$(RM) config.ml

clean:: dummy
	$(RM) *~ \#*\#
	$(RM) $(EXE) $(EXE_BYTE) $(EXE_GTK) $(EXE_GTK_BYTE)
	$(RM) cgtest cgtest.ml-test
	$(RM) *.cm* *.a *.o *.annot *.odoc *.dot *.ps

.depend depend:
	$(RM) .depend
	$(OCAMLDEP) *.ml *.mli > .depend

dummy:

include .depend

#################
# Installation
#################
install: all
	$(OCAMLFIND) install $(PACKAGE) ../META \
		$(LIB) $(LIB:.cmxa=.cmi) $(LIB:.cmxa=.a) $(LIB:.cmxa=.mli) \
		$(LIB_GTK) $(LIB_GTK:.cmxa=.cmi) $(LIB_GTK:.cmxa=.a) $(LIB_GTK:.cmxa=.mli) \
		$(LIB_BYTE) $(LIB_GTK_BYTE)
	$(CP) $(EXE) $(EXE_BYTE) $(OCAMLBIN)
	$(CP) $(EXE_GTK) $(EXE_GTK_BYTE) $(OCAMLBIN)
	$(CP) $(ODOC_HTML) $(ODOC_TEST) $(ODOC) `$(OCAMLDOC) -customdir `
	$(CP) $(ODOC_HTML_BYTE) $(ODOC_TEST_BYTE) $(ODOC_BYTE) `$(OCAMLDOC) -customdir `

uninstall: dummy
	$(OCAMLFIND) remove $(PACKAGE)

###########################
# additional dependencies
###########################
