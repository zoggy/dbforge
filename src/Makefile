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

PACKAGES=str,config-file,xmlm
GTK_PACKAGES=lablgtk2-extras.configwin,lablgtk2.glade
ALL_PACKAGES=$(PACKAGES),$(GTK_PACKAGES)
OF_FLAGS=-package $(ALL_PACKAGES)
INCLUDES=$(XMLLIGHT_INCLUDES) -I sqml -I dbf -I tmpl

COMPFLAGS=$(INCLUDES) -annot
LINKFLAGS=$(INCLUDES)

# The executables and libraries to produce
DBFORGE=dbforge
DBFORGE_BYTE=$(DBFORGE).byte

DBFORGE_GTK=$(DBFORGE)-gtk
DBFORGE_GTK_BYTE=$(DBFORGE_GTK).byte

LIB_DBF=dbf/dbforge.cmxa
LIB_DBF_BYTE=$(LIB_DBF:.cmxa=.cma)

LIB_DBF_GTK=dbf/dbforge_gtk.cmxa
LIB_DBF_GTK_BYTE=$(LIB_DBF_GTK:.cmxa=.cma)

LIB_SQL=dbf/dbf_sql.cmxa
LIB_SQL_BYTE=$(LIB_SQL:.cmxa=.cma)

LIB_MYSQL=dbf/dbf_mysql.cmxa
LIB_MYSQL_BYTE=$(LIB_MYSQL:.cmxa=.cma)

LIB_SQML=sqml/sqml.cmxa
LIB_SQML_BYTE=$(LIB_SQML:.cmxa=.cma)

TMPL=tmpl-engine
TMPL_BYTE=$(TMPL).byte

# Compilation
#############

LIB_DBF_CMOFILES= \
	dbf/dbf_installation.cmo \
	dbf/dbf_misc.cmo \
	dbf/dbf_sql.cmo \
	dbf/dbf_sql_gen.cmo \
	dbf/dbf_sql_vgen.cmo \
	dbf/dbf_sql_qgen.cmo \
	dbf/dbf_sql_io.cmo

LIB_DBF_CMIFILES= $(LIB_DBF_CMOFILES:.cmo=.cmi)
LIB_DBF_CMXFILES= $(LIB_DBF_CMOFILES:.cmo=.cmx)

DBFORGE_COMMON_GTK_CMOFILES= \
	dbf/dbf_gtk2Misc.cmo \
	dbf/dbf_gladeWidgets.cmo \
	dbf/dbf_dbStores.cmo \
	dbf/dbf_selectWidget.cmo \
	dbf/dbf_dbWidget.cmo \
	dbf/dbf_columnGUI.cmo \
	dbf/dbf_indexGUI.cmo \
	dbf/dbf_virtualTableGUI.cmo \
	dbf/dbf_tableGUI.cmo \
	dbf/dbf_queryGUI.cmo \
	dbf/dbf_gtk.cmo

DBFORGE_CMOFILES= dbf/dbf_main.cmo
DBFORGE_CMXFILES= $(DBFORGE_CMOFILES:.cmo=.cmx)
DBFORGE_CMIFILES= $(DBFORGE_CMOFILES:.cmo=.cmi)

DBFORGE_GTK_CMOFILES=	dbf/dbf_main_gtk.cmo
DBFORGE_GTK_CMXFILES= $(DBFORGE_GTK_CMOFILES:.cmo=.cmx)
DBFORGE_GTK_CMIFILES= $(DBFORGE_GTK_CMOFILES:.cmo=.cmi)

LIB_DBF_GTK_CMOFILES=	$(DBFORGE_COMMON_GTK_CMOFILES) dbf/dbforge_gtk.cmo
LIB_DBF_GTK_CMXFILES=$(LIB_DBF_GTK_CMOFILES:.cmo=.cmx)
LIB_DBF_GTK_CMIFILES=$(LIB_DBF_GTK_CMOFILES:.cmo=.cmi)

LIB_SQL_CMOFILES= \
	dbf/dbf_sql_driver.cmo \
	dbf/dbf_sql_misc.cmo
LIB_SQL_CMIFILES=$(LIB_SQL_CMOFILES:.cmo=.cmi)
LIB_SQL_CMXFILES=$(LIB_SQL_CMOFILES:.cmo=.cmx)

LIB_MYSQL_CMOFILES=dbf/dbf_mysql.cmo
LIB_MYSQL_CMIFILES=$(LIB_MYSQL_CMOFILES:.cmo=.cmi)
LIB_MYSQL_CMXFILES=$(LIB_MYSQL_CMOFILES:.cmo=.cmx)

DBF_LIBS=$(LIB_DBF) $(LIB_DBF_GTK) $(LIB_SQL) $(LIB_MYSQL)
DBF_LIBS_BYTE=$(LIB_DBF_BYTE) $(LIB_DBF_GTK_BYTE) $(LIB_SQL_BYTE) $(LIB_MYSQL_BYTE)

SQML_CMOFILES=\
	sqml/sqml_sqlstx.cmo \
	sqml/sqml_helper_lp.cmo \
	sqml/sqml_parser.cmo \
	sqml/sqml_lexer.cmo \
	sqml/sqml_pp.cmo \
	sqml/sqml.cmo

SQML_CMXFILES= $(SQML_CMOFILES:.cmo=.cmx)
SQML_CMIFILES= $(SQML_CMOFILES:.cmo=.cmi)


TMPL_COMMON_CMOFILES=\
	tmpl/template.cmo \
	tmpl/tmplParser.cmo \
	tmpl/tmplLexer.cmo \
	tmpl/tmplIO.cmo

TMPL_CMOFILES=$(TMPL_COMMON_CMOFILES) \
	tmpl/main.cmo
TMPL_CMXFILES= $(TMPL_CMOFILES:.cmo=.cmx)
TMPL_CMIFILES= $(TMPL_CMOFILES:.cmo=.cmi)

all: opt byte
opt: $(TMPL) $(LIB_SQML) $(DBF_LIBS) $(DBFORGE) $(DBFORGE_GTK)
byte: $(TMPL_BYTE) $(LIB_SQML_BYTE) $(DBF_LIBS_BYTE) $(DBFORGE_BYTE) $(DBFORGE_GTK_BYTE)

$(LIB_DBF): $(LIB_DBF_CMIFILES) $(LIB_DBF_CMXFILES)
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -a -o $@ $(LIB_DBF_CMXFILES)

$(LIB_DBF_BYTE): $(LIB_DBF_CMIFILES) $(LIB_DBF_CMOFILES)
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -a -o $@ $(LIB_DBF_CMOFILES)

$(LIB_DBF_GTK): $(LIB_DBF_GTK_CMIFILES) $(LIB_DBF_GTK_CMXFILES)
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -a -o $@ $(LIB_DBF_GTK_CMXFILES)

$(LIB_DBF_GTK_BYTE): $(LIB_DBF_GTK_CMIFILES) $(LIB_DBF_GTK_CMOFILES)
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -a -o $@ $(LIB_DBF_GTK_CMOFILES)

$(DBFORGE): $(LIB_SQML) $(LIB_DBF) $(DBFORGE_CMIFILES) $(DBFORGE_CMXFILES)
	$(OCAMLFIND) ocamlopt -package $(PACKAGES) -linkpkg -o $@ $(LINKFLAGS) \
	xml-light.cmxa $(LIB_SQML) $(LIB_DBF) $(DBFORGE_CMXFILES)

$(DBFORGE_BYTE): $(LIB_SQML_BYTE) $(LIB_DBF_BYTE) $(DBFORGE_CMIFILES) $(DBFORGE_CMOFILES)
	$(OCAMLFIND) ocamlc $ -package $(PACKAGES) -linkpkg -o $@ $(LINKFLAGS) \
	xml-light.cma $(LIB_SQML_BYTE) $(LIB_DBF_BYTE) $(DBFORGE_CMOFILES)

$(DBFORGE_GTK): $(LIB_SQML) $(LIB_DBF) $(LIB_DBF_GTK) $(DBFORGE_GTK_CMIFILES) $(DBFORGE_GTK_CMXFILES)
	$(OCAMLFIND) ocamlopt -package $(ALL_PACKAGES) -linkpkg -o $@ $(LINKFLAGS) \
	xml-light.cmxa $(LIB_SQML) $(LIB_DBF) $(LIB_DBF_GTK) $(DBFORGE_GTK_CMXFILES)

$(DBFORGE_GTK_BYTE): $(LIB_SQML_BYTE) $(LIB_DBF_BYTE) $(LIB_DBF_GTK_BYTE) \
										$(DBFORGE_GTK_CMIFILES) $(DBFORGE_GTK_CMOFILES)
	$(OCAMLFIND) ocamlc -package $(ALL_PACKAGES) -linkpkg -o $@ $(LINKFLAGS) \
	xml-light.cma $(LIB_SQML_BYTE) $(LIB_DBF_BYTE) $(LIB_DBF_GTK_BYTE) $(DBFORGE_GTK_CMOFILES)

$(LIB_SQL): $(LIB_SQL_CMIFILES) $(LIB_SQL_CMXFILES)
	$(OCAMLFIND) ocamlopt -a -o $@ $(COMPFLAGS) $(LIB_SQL_CMXFILES)

$(LIB_SQL_BYTE): $(LIB_SQL_CMIFILES) $(LIB_SQL_CMOFILES)
	$(OCAMLFIND) ocamlc -a -o $@ $(COMPFLAGS) $(LIB_SQL_CMOFILES)

$(LIB_MYSQL): $(LIB_MYSQL_CMIFILES) $(LIB_MYSQL_CMXFILES)
	$(OCAMLFIND) ocamlopt -a -o $@ $(COMPFLAGS) $(LIB_MYSQL_CMXFILES)

$(LIB_MYSQL_BYTE): $(LIB_MYSQL_CMIFILES) $(LIB_MYSQL_CMOFILES)
	$(OCAMLFIND) ocamlc -a -o $@ $(COMPFLAGS) $(LIB_MYSQL_CMOFILES)

$(TMPL): $(TMPL_CMIFILES) $(TMPL_CMXFILES)
	$(OCAMLFIND) ocamlopt -package str -linkpkg -o $@ $(LINKFLAGS) $(TMPL_CMXFILES)

$(TMPL_BYTE): $(TMPL_CMIFILES) $(TMPL_CMOFILES)
	$(OCAMLFIND) ocamlc -package str -linkpkg -o $@ $(LINKFLAGS) $(TMPL_CMOFILES)

$(LIB_SQML): $(SQML_CMIFILES) $(SQML_CMXFILES)
	$(OCAMLFIND) ocamlopt -a -o $@ $(LINKFLAGS) $(SQML_CMXFILES)
$(LIB_SQML_BYTE): $(SQML_CMIFILES) $(SQML_CMOFILES)
	$(OCAMLFIND) ocamlc -a -custom -o $@ $(LINKFLAGS_BYTE) $(SQML_CMOFILES)

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

clean:: dummy
	$(RM) *~ \#*\#
	$(RM) $(DBFORGE) $(DBFORGE_BYTE) $(DBFORGE_GTK) $(DBFORGE_GTK_BYTE)
	$(RM) $(TMPL) $(TMPL_BYTE)
	(cd dbf && $(RM) *.cm* *.a *.o *.annot)
	(cd sqml && $(RM) *.cm* *.a *.o *.annot)
	(cd tmpl && $(RM) *.cm* *.a *.o *.annot)

.depend depend:
	$(RM) .depend
	$(OCAMLDEP) dbf/*.ml dbf/*.mli sqml/*.ml sqml/*.mli tmpl/*.ml tmpl/*.mli > .depend

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
dbf/dbf_sql_gen.ml: dbf/sql_table.ml.tmpl $(TMPL_BYTE)
	$(ROOT)/src/$(TMPL_BYTE) $< > $@

dbf/dbf_sql_vgen.ml: dbf/sql_vtable.ml.tmpl $(TMPL_BYTE)
	$(ROOT)/src/$(TMPL_BYTE) $< > $@

dbf/dbf_sql_qgen.ml: dbf/sql_query.ml.tmpl $(TMPL_BYTE)
	$(ROOT)/src/$(TMPL_BYTE) $< > $@

dbf/dbf_gladeWidgets.ml: dbf/dbforge.glade
	$(LABLGLADECC) -hide-default $< > $@

dbf/dbf_mysql.cmi: dbf/dbf_mysql.mli
	$(OCAMLFIND) ocamlc -c $(COMPFLAGS) -package mysql $<

dbf/dbf_mysql.cmx: dbf/dbf_mysql.ml
	$(OCAMLFIND) ocamlopt -c $(COMPFLAGS) -package mysql $<

dbf/dbf_mysql.cmo: dbf/dbf_mysql.ml
	$(OCAMLFIND) ocamlc -c $(COMPFLAGS) -package mysql $<
