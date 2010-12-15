
BASEDIR=gps-5.0.0
SRC=${BASEDIR}/gnatlib/gnat_src



all: sweep action

include ${BASEDIR}/Makefile.gnat
include ${BASEDIR}/gnatlib/Makefile.gnat 

sweep:
	/bin/rm -f ${BASEDIR}/.gitignore
	/bin/rm -f ${BASEDIR}/.cvsignore
	/bin/rm -rf `find ${BASEDIR} -type d -name .svn`
	

action:
	mkdir -p ${SRC}
.for p in ${GNAT_SOURCES}
	@if test -e ../gcc/ada/${p}; \
	  then cp ../gcc/ada/${p} ${SRC}; \
	  else echo "Not in gcc: ${p}"; \
	fi
.endfor
.for p in ${GNAT_SOURCES_FOR_GNATCOLL}
	@if test -e ../gcc/ada/${p}; \
	  then cp ../gcc/ada/${p} ${SRC}; \
	  else echo "Not in gcc: ${p}"; \
	fi
.endfor

