#!/bin/sh

TRUNK=4.6
SNAPSHOT=20101204
ROOTDIR=gcc-$TRUNK-$SNAPSHOT

rm -rf $ROOTDIR
bunzip2 -c gcc-core-$TRUNK-$SNAPSHOT.tar.bz2 | gtar -xf -
(cd gcc-$TRUNK-$SNAPSHOT && bunzip2 -c ../gnat-aux-$TRUNK-$SNAPSHOT.tar.bz2 | gtar -xf -)

TRASH_CONF_DIRS="
	alpha 
	arc 
	arm 
	avr
	bfin
	cris
	crx
	fr30
	frv
	h8300
	ia64
	iq2000
	lm32
	m32c
	m32r
	m68hc11
	m68k
	mcore
	mep
	microblaze
	mips
	mmix
	mn10300
	moxie
	pa
	pdp11
	picochip
	rs6000
	rx
	s390
	score
	sh
	sparc
	spu
	stormy16
	v850
	vax
	vms
	xtensa"

TRASH_LANG_DIRS="go libgo gcc/go gcc/po"

TRASH_LIBGCC_CONF_DIRS="
	mmix
	microblaze
	ia64
	arm
	rs6000
	s390
	alpha
	mips
	rx
	lm32
	moxie
	frv
	sh
	sparc
	avr"

for lastdir in $TRASH_CONF_DIRS
do
	rm -rf $ROOTDIR/gcc/config/$lastdir
done
for lastdir in $TRASH_LIBGCC_CONF_DIRS
do
	rm -rf $ROOTDIR/libgcc/config/$lastdir
done
for lastdir in $TRASH_LANG_DIRS
do
	rm -rf $ROOTDIR/$lastdir
done

gtar -cf gnat-aux-$SNAPSHOT.tar gcc-$TRUNK-$SNAPSHOT
bzip2 gnat-aux-$SNAPSHOT.tar
