#!/bin/sh
# This script will copy the bare minimum set of files from a repository of
# of the trunk of the gcc repository, a set sufficient to build the Ada and
# C languages.  The ability to build C++ will be maintained, but this will
# have to be added externally.

SNAPSHOT=20110304
TRUNKVER=4.6
GITREPOS=/export/home/marino/shallow_gcc
DRACOREPOS=/export/home/marino/draco/trunk

ROOT_FILES="
   config-ml.in
   config.guess
   config.rpath
   config.sub
   configure
   configure.ac
   depcomp
   install-sh
   libtool-ldflags
   libtool.m4
   ltgcc.m4
   ltmain.sh
   ltoptions.m4
   ltsugar.m4
   ltversion.m4
   Makefile.def
   Makefile.in
   Makefile.tpl
   missing
   mkdep
   mkinstalldirs
   move-if-change
   symlink-tree
   ylwrap"

COMPLETE_DIRS="
   libiberty
   libdecnumber
   libquadmath
   zlib
   lto-plugin
   contrib
   fixincludes
   libmudflap
   include
   intl
   config
   libcpp
   gnattools
   libada"

GCC_DIRS="
   doc
   c-family
   lto
   ginclude
   config/i386
   config/soft-fp"

LIBGCC_DIRS="
   i386/32
   i386/64
   libbid"

SPECIAL_DIRS="
   gcc
   gcc/config
   libgcc
   libgcc/config
   libgcc/config/i386"

BASEDIR=gcc-$TRUNKVER-$SNAPSHOT
TARBALL=gnat-aux-$SNAPSHOT.tar
rm -rf $BASEDIR
mkdir $BASEDIR
mkdir $BASEDIR/gcc
mkdir $BASEDIR/gcc/config
mkdir $BASEDIR/libgcc
mkdir $BASEDIR/libgcc/config
mkdir $BASEDIR/libgcc/config/i386

for single in $ROOT_FILES
do
   cp $GITREPOS/$single $BASEDIR/
done

for comdir in $COMPLETE_DIRS
do
   cp -r $GITREPOS/$comdir $BASEDIR/
done

for comdir in $GCC_DIRS
do
   cp -r $GITREPOS/gcc/$comdir $BASEDIR/gcc/$comdir
done

for comdir in $LIBGCC_DIRS
do
   cp -r $GITREPOS/libgcc/config/$comdir $BASEDIR/libgcc/config/$comdir
done

# populate base/* standard files
for comdir in $SPECIAL_DIRS
do
   gfind $GITREPOS/$comdir/ -maxdepth 1 -type f -exec cp {} $BASEDIR/$comdir/ \;
done

#overwrite everything with draco repository
cp -r $DRACOREPOS/* $BASEDIR/

#apply flux patches
gpatch $BASEDIR/gcc/configure < $DRACOREPOS/../misc/gcc_flux_patches/patch_gcc_configure

#now create a compressed tarball (tar.bz2)
rm -f $TARBALL $TARBALL.bz2
gtar -cf $TARBALL $BASEDIR
bzip2 $TARBALL

