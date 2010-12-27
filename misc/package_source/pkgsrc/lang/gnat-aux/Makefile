# $NetBSD$
#

DISTNAME=	gnat-aux-${SNAPSHOT}
#PKGREVISION=
CATEGORIES=	lang
MASTER_SITES=	http://downloads.dragonlace.net/src/ \
		http://dragonlace.mirrors.ada.cx/src/
EXTRACT_SUFX=	.tar.bz2

MAINTAINER=	draco@marino.st
HOMEPAGE=	http://www.dragonlace.net/
COMMENT=	GNAT Ada compiler based on GCC ${GCC_BRANCH}
LICENSE=	gnu-gpl-v3 AND gnu-lgpl-v3

SNAPSHOT=	20101221
GCC_BRANCH=     4.6
GCC_POINT=      0

PKG_DESTDIR_SUPPORT=	user-destdir

USE_TOOLS+=		gmake sed flex bison gm4 perl makeinfo bzip2
USE_BUILTIN.iconv=	no
BUILD_DEPENDS+=		dejagnu>=1.4:../../devel/dejagnu

.include "../../mk/bsd.prefs.mk"

# Requires bootstrap compiler, which is only available for NetBSD i386/AMD64
# and DragonFly i386/AMD64.  Actually bootstrap compilers for OpenBSD and
# FreeBSD also exist, but native packages will be available for those ports.

ONLY_FOR_PLATFORM=	DragonFly-*-*    \
			NetBSD-5*-i386   \
                        NetBSD-5*-x86_64

BLD_TARGET=		${MACHINE_ARCH}-aux-${LOWER_OPSYS}${OS_VERSION}
THREAD_MODEL=		posix
OS_LABEL4VERS=		[pkgsrc]
BOOTSTRAP_COMPILER=	NOT_REQUIRED
INTENDED_COMPILER=	NATIVE
BOOTSTRAP_TRIPLET=      NOT SET
FULL_GNATGCC=		NOT SET
FULL_PATH=		/sbin:/bin:/usr/sbin:/usr/bin:${PREFIX}/sbin:${PREFIX}/bin


.if ${OPSYS} == "DragonFly"
.if ${MACHINE_ARCH} == "i386"
   OS_LABEL4VERS=	[DragonFly]
   BOOTSTRAP_TRIPLET=	i386-aux-dragonfly2.8/4.6.0
.endif
.if ${MACHINE_ARCH} == "x86_64"
   OS_LABEL4VERS=	[DragonFly64]
   BOOTSTRAP_TRIPLET=	x86_64-aux-dragonfly2.8/4.6.0
.endif
BOOTSTRAP_COMPILER=	gnat-aux-460.${MACHINE_ARCH}.dragonfly.tar.bz2
.endif


.if ${OPSYS} == "NetBSD"
.if ${MACHINE_ARCH} == "i386"
   OS_LABEL4VERS=	[NetBSD]
   BOOTSTRAP_TRIPLET=	i386-aux-netbsd5.1/4.6.0
   BLD_TARGET=		${MACHINE_ARCH}-aux-netbsdelf${OS_VERSION}
.endif
.if ${MACHINE_ARCH} == "x86_64"
   OS_LABEL4VERS=	[NetBSD64]
   BOOTSTRAP_TRIPLET=	x86_64-aux-netbsd5.1/4.6.0
.endif
BOOTSTRAP_COMPILER=	gnat-aux-460.${MACHINE_ARCH}.netbsd.tar.bz2
.endif



# Let's build this without bash
CONFIG_SHELL=		${TOOLS_PATH.sh}


# define some important directories and files
WRKSRC=			${WRKDIR}/build
GCC_WRKSRC=		${WRKDIR}/gcc-${GCC_BRANCH}-${SNAPSHOT}
PKG_PREFIX=		${PREFIX}
CONFIGURE_SCRIPT=	${GCC_WRKSRC}/configure
BOOTSTRAP_PREFIX=	${WRKDIR}/bootstrap
MAN7=			fsf-funding.7 gfdl.7 gpl.7
SINFO=			share/info
SMAN1=			share/man/man1
SMAN7=			share/man/man7
LPATH=			lib/gcc/${BLD_TARGET}/${GCC_BRANCH}.${GCC_POINT}
LEPATH=			libexec/gcc/${BLD_TARGET}/${GCC_BRANCH}.${GCC_POINT}
REVFILE= 		${GCC_WRKSRC}/gcc/REVISION
LELIST=			cc1 collect2 gnat1 lto1


# The bootstrap compiler is at least 28M these days.  Additionally, we would
# prefer to use a native Ada-capable compiler if it were available for two
# reasons:
#    1) It takes a long time to download a file this big and
#    2) Over time, the libraries evolve, breaking the bootstrap
# For the latter reason, a symbolic link may have to be established by this
# script if a latter version of the library is available, but only if the
# bootstrap is determined necessary.


# testing if gnatgcc, gnatlink, gnatbind and gnatmake are in standard location
# exists function will NOT expand any variable, so it has to be hard-coded
.if exists(/usr/pkg/bin/gnatgcc)
.if exists(/usr/pkg/bin/gnatbind)
.if exists(/usr/pkg/bin/gnatlink)
.if exists(/usr/pkg/bin/gnatmake)
FULL_GNATGCC=${PREFIX}/bin/gnatgcc
.endif #gnatmake
.endif #gnatlink
.endif #gnatbind
.endif #gnatgcc


.if ${FULL_GNATGCC} == "NOT SET"
# here we know we need to install the bootstrap compiler
INTENDED_COMPILER=BOOTSTRAP
FULL_GNATGCC=${BOOTSTRAP_PREFIX}/bin/gnatgcc
DISTFILES= ${DISTNAME}${EXTRACT_SUFX} ${BOOTSTRAP_COMPILER}
FULL_PATH=${BOOTSTRAP_PREFIX}/bin:/sbin:/bin:/usr/sbin:/usr/bin:${PREFIX}/sbin:${PREFIX}/bin
.endif


# For pkg maintenance, uncomment to invoke "bmake mdi"
#DISTFILES=${DISTNAME}${EXTRACT_SUFX} \
#  gnat-aux-460.i386.dragonfly.tar.bz2 \
#  gnat-aux-460.x86_64.dragonfly.tar.bz2 \
#  gnat-aux-460.i386.netbsd.tar.bz2 \
#  gnat-aux-460.x86_64.netbsd.tar.bz2


# establish ada-aware compiler for use
MY_CONFIGURE_ENV=	CC=${FULL_GNATGCC} PATH=${FULL_PATH} CONFIG_SHELL=${CONFIG_SHELL}
MY_MAKE_ENV=		PATH=${FULL_PATH}


# The standard configuration options
CONFIGURE_ARGS=		--enable-languages=c,ada
CONFIGURE_ARGS+=	--build=${BLD_TARGET}
CONFIGURE_ARGS+=	--program-prefix=gnat
CONFIGURE_ARGS+=	--prefix=${PREFIX}
CONFIGURE_ARGS+=	--with-system-zlib
CONFIGURE_ARGS+=	--with-gmp=${PKG_PREFIX}
CONFIGURE_ARGS+=	--with-mpfr=${PKG_PREFIX}
CONFIGURE_ARGS+=	--with-mpc=${PKG_PREFIX}
CONFIGURE_ARGS+=	--with-libiconv-prefix=${PKG_PREFIX}
CONFIGURE_ARGS+=	--enable-shared
CONFIGURE_ARGS+=	--enable-threads=${THREAD_MODEL}
CONFIGURE_ARGS+=	--disable-bootstrap
CONFIGURE_ARGS+=	--disable-libgomp
CONFIGURE_ARGS+=	--disable-libssp
CONFIGURE_ARGS+=	--disable-nls


# The standard make options
MAKE_ARGS=		MAKEINFOFLAGS="--no-split"


# Automatic package list generation
GENERATE_PLIST=  cd ${PREFIX};
GENERATE_PLIST+= ${FIND} bin/gnat* | ${SORT};
GENERATE_PLIST+= ${FIND} bin/${BLD_TARGET}* | ${SORT};
GENERATE_PLIST+= ${FIND} ${LPATH}/*     -type f | ${SORT};
GENERATE_PLIST+= ${FIND} ${LEPATH}/*    -type f | ${SORT};
GENERATE_PLIST+= ${FIND} ${SINFO}/gnat* -type f | ${SORT};
GENERATE_PLIST+= ${FIND} ${SMAN1}/gnat* -type f | ${SORT};
.for k in ${MAN7}
GENERATE_PLIST+= ${FIND} ${SMAN7}/${k};
.endfor



post-extract:
	# Personalize GNAT for each different machine
	@${ECHO} "-=> GNAT AUX ${OS_LABEL4VERS}" > ${REVFILE}

.if ${INTENDED_COMPILER} == "BOOTSTRAP"
	${MKDIR} ${BOOTSTRAP_PREFIX}
	mv ${BOOTSTRAP_PREFIX}/../bin ${BOOTSTRAP_PREFIX}
	mv ${BOOTSTRAP_PREFIX}/../lib ${BOOTSTRAP_PREFIX}
	mv ${BOOTSTRAP_PREFIX}/../libexec ${BOOTSTRAP_PREFIX}

	# Protect bootstrap compiler from future breakage
	# gmp, mpc, mpfr, and iconv should already be installed at this point
	# Hardlinks are used to protect against dangling references
	# which confuse makefiles
.if exists(/usr/pkg/lib/libmpc.so) && !exists(/usr/pkg/lib/libmpc.so.2)
	$(LN) /usr/pkg/lib/libmpc.so /usr/pkg/lib/libmpc.so.2
.endif
.if exists(/usr/pkg/lib/libgmp.so) && !exists(/usr/pkg/lib/libgmp.so.10)
	$(LN) /usr/pkg/lib/libgmp.so /usr/pkg/lib/libgmp.so.10
.endif
.if exists(/usr/pkg/lib/libmpfr.so) && !exists(/usr/pkg/lib/libmpfr.so.4)
	$(LN) /usr/pkg/lib/libmpfr.so /usr/pkg/lib/libmpfr.so.4
.endif
.if exists(/usr/pkg/lib/libiconv.so) && !exists(/usr/pkg/lib/libiconv.so.2)
	$(LN) /usr/pkg/lib/libiconv.so /usr/pkg/lib/libiconv.so.2
.endif
.endif



do-configure:
	#late switch of compiler to fool default depends checking
	${SETENV} CC=${FULL_GNATGCC}

	#reset timestamps
	cd ${GCC_WRKSRC}; contrib/gcc_update --touch
	${RM} -f ${GCC_WRKSRC}/gcc/*/*.info*
	${TOUCH} ${GCC_WRKSRC}/gcc/cstamp-h.in

	${MKDIR} ${WRKSRC}
	cd ${WRKSRC} && ${SETENV} ${MY_CONFIGURE_ENV} ${CONFIG_SHELL} \
	${CONFIGURE_SCRIPT} ${CONFIGURE_ARGS}


do-build:
	cd ${WRKSRC} && ${SETENV} ${MY_MAKE_ENV} ${GMAKE} all ${MAKE_ARGS}


do-test: build
	cd ${WRKSRC} && ${SETENV} ${MY_MAKE_ENV} ${GMAKE} -sk check-ada


do-install:
	cd ${WRKSRC} && ${SETENV} ${MY_MAKE_ENV} ${GMAKE} install


strip-exec:
	${STRIP} ${PREFIX}/bin/gnat*
.for k in ${LELIST}
	${STRIP} ${PREFIX}/${LEPATH}/${k}
.endfor



.include "../../devel/zlib/buildlink3.mk"
.include "../../devel/gmp/buildlink3.mk"
.include "../../math/mpfr/buildlink3.mk"
.include "../../math/mpcomplex/buildlink3.mk"
.include "../../converters/libiconv/buildlink3.mk"
.include "../../mk/bsd.pkg.mk"
