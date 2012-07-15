# New ports collection makefile for:  gcc-aux
# Date created:                       15 July 2012
# Whom:                               John Marino <draco@marino.st>
#
# $FreeBSD$
#

PORTNAME=	gcc-aux
PORTVERSION=	${SNAPSHOT}
CATEGORIES=	lang
MASTER_SITES=	http://downloads.dragonlace.net/src/:boot \
		http://dragonlace.mirrors.ada.cx/src/:boot \
		${MASTER_SITE_GCC}
MASTER_SITE_SUBDIR=	releases/gcc-${GCC_VERSION}
DISTFILES=	gcc-${GCC_VERSION}.tar.bz2

MAINTAINER=	draco@marino.st
COMMENT=	Version of GCC ${GCC_BRANCH} with full Ada support

.include "${.CURDIR}/Makefile.common"

LANGS=			c
APPLY_DIFFS=		core
INTENDED_COMPILER=	NATIVE
BOOTSTRAP_COMPILER=	NOT_REQUIRED
BOOTSTRAP_TRIPLET=	NOT_SET
FULL_GNATGCC=		NOT_SET

OPTIONS=	ADA        "Build Ada language" on
OPTIONS+=	CXX        "Build C++ language" on
OPTIONS+=	FORT       "Build Fortran language" on
OPTIONS+=	OBJC       "Build Objective-C language" on
OPTIONS+=	STATIC     "Build with no shared libraries other than libc" off
OPTIONS+=	TESTSUITE  "Activate test support" off
OPTIONS+=	NLS        "Native Language Support" off

.include <bsd.port.pre.mk>
.include <bsd.port.options.mk>

.if ${ARCH} == "amd64"
BLD_TARGET=		x86_64-aux-freebsd${OSREL}
OS_LABEL4VERS=		[FreeBSD64]
BOOTSTRAP_TRIPLET=	x86_64-bootstrap-freebsd8.1/4.6.0
BOOTSTRAP_COMPILER=	gnat-bootstrap.x86_64.freebsd.tar.bz2
.else
BLD_TARGET=		i386-aux-freebsd${OSREL}
OS_LABEL4VERS=		[FreeBSD]
BOOTSTRAP_TRIPLET=	i386-bootstrap-freebsd8.0/4.6.0
BOOTSTRAP_COMPILER=	gnat-bootstrap.i386.freebsd.tar.bz2
.endif
PLIST_SUB=		GNU_HOST=${BLD_TARGET}
PLIST_SUB+=		GCC_VER=${GCC_VERSION}

WRKSRC=			${WRKDIR}/gcc-${GCC_VERSION}
BUILDDIR=		${WRKDIR}/build
CFG_SCRIPT=		${WRKSRC}/configure
REVFILE=		${WRKSRC}/gcc/REVISION
BOOTSTRAP_PREFIX=	${WRKDIR}/bootstrap
PKG_PREFIX=		${PREFIX}
LPATH=			lib/gcc/${BLD_TARGET}/${GCC_VERSION}
LEPATH=			libexec/gcc/${BLD_TARGET}/${GCC_VERSION}
STICONV=		${PREFIX}/lib/libiconv.a
STINTL= 		\$${top_builddir}/../intl/libintl.a
USE_LDCONFIG=		${PREFIX}/lib/gcc
MAN1=			gnatcpp.1 gnatgcc.1 gnatgcov.1
MAN1PREFIX=		${PREFIX}/share
MAN7PREFIX=		${PREFIX}/share
NO_MANCOMPRESS=		yes

# If we find gnatgcc and friends in standard location, then we'll use this
# compiler instead of downloading the bootstrap.
.if exists (${PREFIX}/gcc-aux/bin/ada)
.if exists (${PREFIX}/gcc-aux/bin/gnatbind)
.if exists (${PREFIX}/gcc-aux/bin/gnatlink)
.if exists (${PREFIX}/gcc-aux/bin/gnatmake)
FULL_GNATGCC=${PREFIX}/gcc-aux/bin/ada
FULL_PATH=/sbin:/bin:/usr/sbin:/usr/bin:${PREFIX}/gcc-aux/bin:${PREFIX}/bin
.endif
.endif
.endif
.endif

# If FULL_GNATGCC is not set, we'll request the bootstrap compiler
.if ${FULL_GNATGCC} == "NOT_SET"
INTENDED_COMPILER=BOOTSTRAP
FULL_GNATGCC=${BOOTSTRAP_PREFIX}/bin/gnatgcc
FULL_PATH=${BOOTSTRAP_PREFIX}/bin:/sbin:/bin:/usr/sbin:/usr/bin:${PREFIX}/bin
DISTFILES+=	${BOOTSTRAP_COMPILER}:boot
.endif

# for port maintenance, invoke "make makesum PLUS_BOOTSTRAPS=1"
.if defined(PLUS_BOOTSTRAPS)
DISTFILES=gcc-${GCC_VERSION}.tar.bz2 \
   gnat-bootstrap.i386.freebsd.tar.bz2:boot \
   gnat-bootstrap.x86_64.freebsd.tar.bz2:boot
.endif

###########
##  Ada  ##
###########

.if defined(WITH_ADA)
LANGS+=		ada
APPLY_DIFFS+=	ada
PLIST_SUB+=	ADA=""
.else
PLIST_SUB+=	ADA="@comment "
.endif

###########
##  C++  ##
###########

.if defined(WITH_CXX)
LANGS+=		c++
APPLY_DIFFS+=	cxx
MAN1+=		gnatg++.1
LCXXPATH=	include/c++/${GCC_VERSION}
LSHRPATH=	share/gcc-${GCC_VERSION}
PLIST_SUB+=	CXX=""
.else
PLIST_SUB+=	CXX="@comment "
.endif

###############
##  Fortran  ##
###############

.if defined(WITH_FORT)
LANGS+=		fortran
APPLY_DIFFS+=	fortran
MAN1+=		gnatgfortran.1
EXTRA_CONFIG+=	--enable-libquadmath
PLIST_SUB+= 	FRT=""
.else
EXTRA_CONFIG+=	--disable-libquadmath
PLIST_SUB+= 	FRT="@comment "
.endif

###################
##  Objective-C  ##
###################

.if defined(WITH_OBJC)
LANGS+=		objc
.endif

#########################
##  Testsuite Support  ##
#########################

.if defined(WITH_TESTSUITE)
BUILD_DEPENDS=	dejagnu>=1.4:${PORTSDIR}/misc/dejagnu
APPLY_DIFFS+=	ada-testsuite
APPLY_DIFFS+=	fortran-testsuite
APPLY_DIFFS+=	cxx-testsuite
APPLY_DIFFS+=	gcc-testsuite
.endif

#################################
##  NATIONAL LANGUAGE SUPPORT  ##
#################################

.if defined(WITH_NLS)
BUILD_DEPENDS=	gettext>=0.18:${PORTSDIR}/devel/gettext
EXTRA_CONFIG+= --enable-nls
PLIST_SUB+= 	NLS=""
.else
EXTRA_CONFIG+= --disable-nls
PLIST_SUB+= 	NLS="@comment "
.endif

###############################
##  STATICALLY BUILT OPTION  ##
###############################
#
# The "static" option is handled in the post-extract phase.

# Establish Ada-aware compiler for use
ADA_CONFIGURE_ENV=	CC=${FULL_GNATGCC}
ADA_CONFIGURE_ENV+=	PATH=${FULL_PATH}
ADA_MAKE_ENV=		PATH=${FULL_PATH}
ADA_MAKE_ENV+=		ICONVPREFIX=${PREFIX}
ADA_MAKE_ENV+=		LD_LIBRARY_PATH=${BUILDDIR}/gcc

# The standard configuration options
CONFIGURE_ARGS=		--enable-languages=${LANGS:Q}
CONFIGURE_ARGS+=	--build=${BLD_TARGET}
CONFIGURE_ARGS+=	--prefix=${PREFIX}
CONFIGURE_ARGS+=	--with-system-zlib
CONFIGURE_ARGS+=	--with-gmp=${PKG_PREFIX}
CONFIGURE_ARGS+=	--with-mpfr=${PKG_PREFIX}
CONFIGURE_ARGS+=	--with-mpc=${PKG_PREFIX}
CONFIGURE_ARGS+=	--with-libiconv-prefix=${PKG_PREFIX}
CONFIGURE_ARGS+=	--enable-shared
CONFIGURE_ARGS+=	--enable-threads=posix
CONFIGURE_ARGS+=	--disable-bootstrap
CONFIGURE_ARGS+=	--disable-libmudflap
CONFIGURE_ARGS+=	--disable-libgomp
CONFIGURE_ARGS+=	--disable-libssp
CONFIGURE_ARGS+=	${EXTRA_CONFIG}

post-extract:
	# Personalize GNAT for each different machine
	@${ECHO} "-=> GNAT AUX ${OS_LABEL4VERS}" > ${REVFILE}

	# Create new directories in preparation of applying diff files
.if defined(WITH_CXX)
	${MKDIR} ${WRKSRC}/libstdc++-v3/config/locale/dragonfly
	${MKDIR} ${WRKSRC}/libstdc++-v3/config/os/bsd/dragonfly
.endif

	# Apply required composite diff files
.for suffix in ${APPLY_DIFFS}
	@${ECHO} "Applying composite patch diff-${suffix}"
	@${PATCH} -d ${WRKSRC} -s -E < ${FILESDIR}/diff-${suffix}
.endfor

.if defined(WITH_STATIC)
	@${ECHO} "Reconfiguring GCC Makefile to build compiler statically"
	@${PATCH} -d ${WRKSRC} -s -E < ${FILESDIR}/diff-static-version
.if defined(WITH_NLS)
	@perl -pi -e 's;\@LIBICONV\@;${STICONV};' \
	  ${WRKSRC}/gcc/Makefile.in \
	  ${WRKSRC}/libcpp/Makefile.in \
	  ${WRKSRC}/gcc/ada/gcc-interface/Makefile.in
.else
	@perl -pi -e 's;\@LIBINTL\@;${STINTL} ${STICONV};' ${WRKSRC}/intl/config.intl.in
	@perl -pi -e 's;\@LIBICONV\@;;' \
	  ${WRKSRC}/gcc/Makefile.in \
	  ${WRKSRC}/libcpp/Makefile.in \
	  ${WRKSRC}/gcc/ada/gcc-interface/Makefile.in
.endif
.endif

.if ${INTENDED_COMPILER} == "BOOTSTRAP"
	${MKDIR} ${BOOTSTRAP_PREFIX}
	${MV} ${BOOTSTRAP_PREFIX}/../bin ${BOOTSTRAP_PREFIX}
	${MV} ${BOOTSTRAP_PREFIX}/../lib ${BOOTSTRAP_PREFIX}
	${MV} ${BOOTSTRAP_PREFIX}/../libexec ${BOOTSTRAP_PREFIX}

	# Bootstrap compiler has statically linked z, gmp, mpc, mpfr and iconv
	# The only shared lib is libc.so.7, so it should work for a long time.

.if ${OSVERSION} > 900000
	# Since June 7, bootstrap compiler's include-fixed headers have been
	# incompatible with FreeBSD 9.0-CURRENT
	${RM} -rf ${BOOTSTRAP_PREFIX}/lib/gcc/${BOOTSTRAP_TRIPLET}/include-fixed
.endif
.endif

do-configure:
	${MKDIR} ${BUILDDIR}
	cd ${BUILDDIR} && ${SETENV} ${ADA_CONFIGURE_ENV} \
	${CFG_SCRIPT} ${CONFIGURE_ARGS}

do-build:
	cd ${BUILDDIR} && ${SETENV} ${ADA_MAKE_ENV} ${GMAKE} ${_MAKE_JOBS} all

test: build test-ada test-fortran test-objc test-cxx test-c

test-ada:
.if defined(WITH_ADA) && defined(WITH_TESTSUITE)
	cd ${BUILDDIR} && ${SETENV} ${ADA_MAKE_ENV} ${GMAKE} -sk check-ada
.endif

test-fortran:
.if defined(WITH_FORT) && defined(WITH_TESTSUITE)
	cd ${BUILDDIR} && ${SETENV} ${ADA_MAKE_ENV} ${GMAKE} -sk check-fortran
.endif

test-objc:
.if defined(WITH_OBJC) && defined(WITH_TESTSUITE)
	cd ${BUILDDIR} && ${SETENV} ${ADA_MAKE_ENV} ${GMAKE} -sk check-objc
.endif

test-cxx:
.if defined(WITH_CXX) && defined(WITH_TESTSUITE)
	# libstdc++ testsuite will break every time, TRUE used to force continuation
	cd ${BUILDDIR} && ${SETENV} ${ADA_MAKE_ENV} ${GMAKE} -sk check-c++ || true
.endif

test-c:
.if defined(WITH_TESTSUITE)
	cd ${BUILDDIR} && ${SETENV} ${ADA_MAKE_ENV} ${GMAKE} -sk check-c
.endif

do-install:
	cd ${BUILDDIR} && ${SETENV} ${ADA_MAKE_ENV} ${GMAKE} install-strip DESTDIR=${DESTDIR}

post-install:
	${RM} -f ${WRKDIR}/PLIST.lib
.for d in ${LPATH} ${LEPATH} ${LCXXPATH} ${LSHRPATH}
	cd ${PREFIX} ; ${FIND} $d \( -type f -or -type l \) | ${SORT} >> ${WRKDIR}/PLIST.lib
	cd ${PREFIX} ; ${FIND} $d  -type d | ${SORT} -r | \
	   ${SED} -e 's/^/@dirrm /g' >> ${WRKDIR}/PLIST.lib
.endfor
	${ECHO_CMD} "@unexec ${RMDIR} %D/lib/gcc/${BLD_TARGET}     2>/dev/null || true" >> ${WRKDIR}/PLIST.lib
	${ECHO_CMD} "@unexec ${RMDIR} %D/lib/gcc                   2>/dev/null || true" >> ${WRKDIR}/PLIST.lib
	${ECHO_CMD} "@unexec ${RMDIR} %D/libexec/gcc/${BLD_TARGET} 2>/dev/null || true" >> ${WRKDIR}/PLIST.lib
	${ECHO_CMD} "@unexec ${RMDIR} %D/libexec/gcc               2>/dev/null || true" >> ${WRKDIR}/PLIST.lib
	${ECHO_CMD} "@unexec ${RMDIR} %D/include/c++               2>/dev/null || true" >> ${WRKDIR}/PLIST.lib

	cd ${WRKDIR}; ${SED} -i -e "/PLIST.lib/ r PLIST.lib" ${TMPPLIST}
.if ${OSVERSION} > 900000
	@${ECHO_MSG} "========================================================"
	@${ECHO_MSG} " NOTICE REGARDING ADA TASKING ON FREEBSD 9.x:"
	@${ECHO_MSG} " Due to a new resource check in the threading library"
	@${ECHO_MSG} " starting with FreeBSD 9.0, an exiting task panics with"
	@${ECHO_MSG} " the message 'thread exits with resources held!'."
	@${ECHO_MSG} " Essentially this behavior breaks Ada tasking and 23"
	@${ECHO_MSG} " ACATS tests fail on FreeBSD 9.x. A solution is being"
	@${ECHO_MSG} " sought, but the necessary patches will not be trivial."
	@${ECHO_MSG} "========================================================"
.endif

.include <bsd.port.post.mk>
