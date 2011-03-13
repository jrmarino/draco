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

# del libquadmath
# del libmudflap

COMPLETE_DIRS="
   libiberty
   libdecnumber
   libstdc++-v3
   zlib
   lto-plugin
   contrib
   fixincludes
   include
   intl
   config
   libcpp
   gnattools
   libada"

GCC_DIRS="
   doc
   c-family
   cp
   lto
   ginclude
   testsuite/g++.dg
   testsuite/gcc.dg
   testsuite/c-c++-common
   config/i386
   config/soft-fp"

LIBGCC_DIRS="
   i386/32
   i386/64
   libbid"

TESTTARGET_DIRS="
   i386
   x86_64"

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
mkdir $BASEDIR/gcc/testsuite
mkdir $BASEDIR/gcc/testsuite/gcc.target
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

for comdir in $TESTTARGET_DIRS
do
   cp -r $GITREPOS/gcc/testsuite/gcc.target/$comdir $BASEDIR/gcc/testsuite/gcc.target/$comdir
done

# populate base/* standard files
for comdir in $SPECIAL_DIRS
do
   gfind $GITREPOS/$comdir/ -maxdepth 1 -type f -exec cp {} $BASEDIR/$comdir/ \;
done

#overwrite everything with draco repository
cp -r $DRACOREPOS/* $BASEDIR/

# Wipeout ChangeLogs
gfind $BASEDIR/ -name ChangeLog\* -type f -exec rm {} \;

#apply flux patches
PATCHARGS="-d $BASEDIR --forward -E -p1"
gpatch $BASEDIR/gcc/configure < $DRACOREPOS/../misc/gcc_flux_patches/patch_gcc_configure
gpatch $PATCHARGS < $DRACOREPOS/../misc/gcc_flux_patches/libstdcxx-testsuite.patch
gpatch $PATCHARGS < $DRACOREPOS/../misc/gcc_flux_patches/libstdc++.exp.patch
gpatch $PATVHARGS < $DROCOREPOS/../misc/gcc_flux_patches/libstdxx_ts_missing_debug_checks.patch

#now create a compressed tarball (tar.bz2)
rm -f $TARBALL $TARBALL.bz2
gtar -cf $TARBALL $BASEDIR
bzip2 $TARBALL

