#################################################################################
#                DBForge                                                        #
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

include master.Makefile

# Compilation
#############

all: src

src: dummy
	cd src && $(MAKE) all

re : depend clean all

# Documentation :
#################
doc: dummy
	cd src && $(MAKE) doc

# myself

master.Makefile: master.Makefile.in src/dbf/dbf_installation.ml.in config.status
	./config.status

config.status: configure
	./config.status --recheck

configure: configure.in
	autoconf

# backup, clean and depend :
############################

distclean: clean
	cd src && $(MAKE) distclean
	$(RM) config.cache config.log config.status master.Makefile \
	src/dbf/dbf_installation.ml src/dbf/META src/sqml/META

clean:: dummy
	$(RM) *~ \#*\#
	cd src && $(MAKE) clean

depend: dummy
	cd src && $(MAKE) depend

dummy:

###########
# Headers
###########
HEADFILES=configure.in configure \
	master.Makefile.in Makefile src/Makefile checkocaml.ml \
	src/*/*.ml src/*/*.mli src/dbf/dbf_installation.ml.in

headers: dummy
	headache -h header -c ~/.headache_config $(HEADFILES)

noheaders: dummy
	headache -r -c ~/.headache_config $(HEADFILES)


#################
# installation
#################

install: dummy
	cd src && $(MAKE) install

# for opam
install-lib: dummy
	cd src && $(MAKE) install-lib

uninstall: dummy
	cd src && $(MAKE) uninstall

# for opam
uninstall-lib: dummy
	cd src && $(MAKE) uninstall-lib


###########
# web site
###########
webdoc:
	cp web/index.html web/style.css ../dbforge-gh-pages/

###########
# archive
###########
archive:
	git archive --prefix=dbforge-$(VERSION)/ HEAD | gzip > ../dbforge-gh-pages/dbforge-$(VERSION).tar.gz

###########################
# additional dependencies
###########################

# DO NOT DELETE
