#!/bin/sh
# This script will copy the bare minimum set of files from a repository of
# of the trunk of the gcc repository, a set sufficient to build the Ada and
# C languages.  The ability to build C++ will be maintained, but this will
# have to be added externally.

SNAPSHOT=20110627
TRUNKVER=4.6
GITREPOS=/export/home/marino/shallow_gcc
DRACOREPOS=/export/home/marino/draco/trunk
BASEDIR=gcc-$TRUNKVER-$SNAPSHOT
PATCHARGS="-d $BASEDIR --forward -E -p1 --no-backup-if-mismatch"


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
   testsuite/c-c++-common
   testsuite/g++.dg
   testsuite/gcc.dg
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

UNWANTED_LIBSTDCXX="
   config/cpu/alpha
   config/cpu/alpha
   config/cpu/arm
   config/cpu/cris
   config/cpu/hppa
   config/cpu/ia64
   config/cpu/m68k
   config/cpu/microblaze
   config/cpu/powerpc
   config/cpu/sh
   config/cpu/sparc
   config/locale/gnu
   config/locale/ieee_1003.1-2001
   config/os/aix
   config/os/bionic
   config/os/djgpp
   config/os/gnu-linux
   config/os/hpux
   config/os/irix
   config/os/mingw32
   config/os/newlib
   config/os/qnx
   config/os/tpf
   config/os/uclibc
   config/os/vxworks"


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

# Wipeout ChangeLogs
gfind $BASEDIR/ -name ChangeLog\* -type f -exec rm {} \;

#prune libstdc++
for comdir in $UNWANTED_LIBSTDCXX
do
   rm -rf $BASEDIR/libstdc++-v3/$comdir
done

#overwrite everything with draco repository
cp -r $DRACOREPOS/* $BASEDIR/

#apply flux patches
gpatch $PATCHARGS < $DRACOREPOS/../misc/gcc_flux_patches/patch_gcc_configure
gpatch $PATCHARGS < $DRACOREPOS/../misc/gcc_flux_patches/patch_gcc_Makefile.in
gpatch $PATCHARGS < $DRACOREPOS/../misc/gcc_flux_patches/patch_configure
gpatch $PATCHARGS < $DRACOREPOS/../misc/gcc_flux_patches/libstdcxx-testsuite.patch
gpatch $PATCHARGS < $DRACOREPOS/../misc/gcc_flux_patches/libstdc++.exp.patch
gpatch $PATCHARGS < $DRACOREPOS/../misc/gcc_flux_patches/libstdxx_ts_missing_debug_checks.patch
gpatch $PATCHARGS < $DRACOREPOS/../misc/gcc_flux_patches/fix_locales.patch
gpatch $PATCHARGS < $DRACOREPOS/../misc/gcc_flux_patches/fix-ja_JP.eucJP.patch
gpatch $PATCHARGS < $DRACOREPOS/../misc/gcc_flux_patches/fix-hong_kong.patch
gpatch $PATCHARGS < $DRACOREPOS/../misc/gcc_flux_patches/fix-norway.patch
gpatch $PATCHARGS < $DRACOREPOS/../misc/gcc_flux_patches/fix-random-locales.patch


#now create a compressed tarball (tar.bz2)
rm -f $TARBALL $TARBALL.bz2
gtar -cf $TARBALL $BASEDIR
bzip2 $TARBALL

