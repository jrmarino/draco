# Created by: John Marino <marino@FreeBSD.org>
# $FreeBSD$

PORTNAME=	gnat-gpl
PORTVERSION=	2015
CATEGORIES=	adacore
MASTER_SITES=	http://downloads.dragonlace.net/src/:boot \
		http://downloads.dragonlace.net/src/:gnat \
		LOCAL/marino:boot 
DISTNAME=	gcc-4.9-gpl-2015-src
DISTFILES=	${DISTNAME}.tar.bz2

MAINTAINER=	marino@FreeBSD.org
COMMENT=	Private version of GNAT GPL ${PORTVERSION}

LICENSE=	GPLv3 GPLv3RLE
LICENSE_COMB=	multi

VALID_CATEGORIES=	adacore

LIB_DEPENDS=	libgmp.so:${PORTSDIR}/math/gmp \
		libmpfr.so:${PORTSDIR}/math/mpfr \
		libmpc.so:${PORTSDIR}/math/mpc

GNU_CONFIGURE=	yes
USES=		gmake iconv libtool perl5
USE_PERL5=	build
ALL_TARGET=	default

LANGS=			c c++ ada
APPLY_DIFFS=		core cxx ada
GARCH=			${ARCH:S/amd64/x86_64/}
BOOTSTRAP_COMPILER=	gnat-bootstrap.${GARCH}.${OPSYS:tl}.tar.bz2
BLD_TARGET=		${GARCH}-adacore-${OPSYS:tl}${OSREL}
OS_LABEL4VERS=		[${OPSYS}${GARCH:M*64:S/amd_//:S/x86_//}]
NO_MTREE=		yes
CPPFLAGS+=		-I${LOCALBASE}/include
LDFLAGS+=		-L${LOCALBASE}/lib

.include <bsd.port.options.mk>

.if ${OPSYS} == FreeBSD
ONLY_FOR_ARCHS=		amd64 i386
MODERN_BINUTILS=	yes
BOOTSTRAP_COMPILER=	ada-bootstrap.${GARCH}.freebsd.100B.tar.bz2
.endif
.if ${OPSYS} == DragonFly
ONLY_FOR_ARCHS=		x86_64
BOOTSTRAP_COMPILER=	ada-bootstrap.${GARCH}.dragonfly.41.tar.bz2
.endif

.if defined(MODERN_BINUTILS)
# Apparently gcc 4.9 needs binutils 2.23 or greater to link Ada correctly
USE_BINUTILS=	yes
RUN_DEPENDS+=	${LOCALBASE}/bin/as:${PORTSDIR}/devel/binutils
EXTRA_CONFIG+=	--with-ld=${LOCALBASE}/bin/ld
EXTRA_CONFIG+=	--with-as=${LOCALBASE}/bin/as
.else
EXTRA_CONFIG+=	--with-ld=/usr/bin/ld
EXTRA_CONFIG+=	--with-as=/usr/bin/as
.endif

BUILDDIR=		${WRKDIR}/build
NEWBSDIR=		${WRKDIR}/newbs/bootstrap
CFG_SCRIPT=		${WRKSRC}/configure
REVFILE=		${WRKSRC}/gcc/REVISION
BOOTSTRAP_PREFIX=	${WRKDIR}/bootstrap
PKG_PREFIX=		${PREFIX}/${PORTNAME}
MANPREFIX=		${PREFIX}/${PORTNAME}/share
BSFILENAME=		ada-bootstrap.${GARCH}.${OPSYS:tl}.${OSREL:S/.//}.tar.bz2

FULL_GNATGCC=${BOOTSTRAP_PREFIX}/bin/gcc
FULL_GNATGXX=${BOOTSTRAP_PREFIX}/bin/g++
FULL_PATH=${BOOTSTRAP_PREFIX}/bin:/sbin:/bin:/usr/sbin:/usr/bin:${PREFIX}/bin
DISTFILES+=	${BOOTSTRAP_COMPILER}:boot

# Establish Ada-aware compiler for use
ADA_CONFIGURE_ENV+=	CC=${FULL_GNATGCC}
ADA_CONFIGURE_ENV+=	CXX=${FULL_GNATGXX}
ADA_CONFIGURE_ENV+=	CFLAGS="${CFLAGS}"
ADA_CONFIGURE_ENV+=	CPPFLAGS="${CPPFLAGS}"
ADA_CONFIGURE_ENV+=	CXXFLAGS="${CXXFLAGS}"
ADA_CONFIGURE_ENV+=	LDFLAGS="${LDFLAGS}"
ADA_CONFIGURE_ENV+=	build_configargs="CPPFLAGS=\"${CPPFLAGS}\""
ADA_CONFIGURE_ENV+=	host_configargs="CPPFLAGS=\"${CPPFLAGS}\""
ADA_CONFIGURE_ENV+=	PATH=${FULL_PATH}
ADA_MAKE_ENV+=		PATH=${FULL_PATH}

# The standard configuration options
ADA_CONFIG_ARGS=	--enable-languages=${LANGS:Q}
ADA_CONFIG_ARGS+=	--build=${BLD_TARGET}
ADA_CONFIG_ARGS+=	--prefix=${PKG_PREFIX:Q}
ADA_CONFIG_ARGS+=	--with-system-zlib
ADA_CONFIG_ARGS+=	--with-gmp=${PREFIX}
ADA_CONFIG_ARGS+=	--with-mpfr=${PREFIX}
ADA_CONFIG_ARGS+=	--with-mpc=${PREFIX}
ADA_CONFIG_ARGS+=	${ICONV_CONFIGURE_ARG}
ADA_CONFIG_ARGS+=	--enable-shared
ADA_CONFIG_ARGS+=	--enable-threads=posix
ADA_CONFIG_ARGS+=	--enable-checking=release
ADA_CONFIG_ARGS+=	--disable-libmudflap
ADA_CONFIG_ARGS+=	--disable-libgomp
ADA_CONFIG_ARGS+=	--disable-libssp
ADA_CONFIG_ARGS+=	--disable-libcilkrts
ADA_CONFIG_ARGS+=	--disable-libitm
ADA_CONFIG_ARGS+=	--disable-nls
ADA_CONFIG_ARGS+=	${EXTRA_CONFIG}

post-extract:
	# Personalize GNAT for each different machine
	@${ECHO} "-=> GNAT AUX ${OS_LABEL4VERS}" > ${REVFILE}
	# Create new directories in preparation of applying diff files
	${MKDIR} ${WRKSRC}/libstdc++-v3/config/locale/dragonfly
	${MKDIR} ${WRKSRC}/libstdc++-v3/config/os/bsd/dragonfly

	# Apply required composite diff files
.for suffix in ${APPLY_DIFFS}
	@${ECHO} "Applying composite patch diff-${suffix}"
	@${PATCH} -d ${WRKSRC} -s -E < ${FILESDIR}/diff-${suffix}
.endfor
	# Update LINK_SPEC to add gcc-aux lib runpath in every binary
	@${ECHO} "Configuring LINK_SPEC runpath"
	@${REINPLACE_CMD} -e 's;\@PREFIX\@;${PREFIX};' \
		-e 's;\@GCCAUX\@;${PORTNAME};' \
		${WRKSRC}/gcc/config/dragonfly.h \
		${WRKSRC}/gcc/config/i386/freebsd64.h \
		${WRKSRC}/gcc/config/i386/freebsd.h
.if defined(MODERN_BINUTILS)
	# Use dwarf4-compatible addr2line (affects FreeBSD only)
	${REINPLACE_CMD} -e 's|/usr/bin/addr2line|${PREFIX}/bin/addr2line|g' \
	  ${WRKSRC}/gcc/ada/traceback_symbolic.c
.endif

do-configure:
	${MKDIR} ${BUILDDIR}
	cd ${BUILDDIR} && ${SETENV} ${ADA_CONFIGURE_ENV} \
		${CFG_SCRIPT} ${ADA_CONFIG_ARGS}

do-build:
	cd ${BUILDDIR} && ${SETENV} ${ADA_MAKE_ENV} \
		${MAKE_CMD} -j${MAKE_JOBS_NUMBER} ${MAKE_ARGS} all

do-install:
	cd ${BUILDDIR} && ${SETENV} ${ADA_MAKE_ENV} \
		${MAKE_CMD} install-strip ${MAKE_ARGS}
	${MV} ${STAGEDIR}${PKG_PREFIX}/share ${WRKDIR}/moved_share

post-stage:
	(cd ${STAGEDIR}${PKG_PREFIX}; ${FIND} * -type d -empty | \
		${SORT} -dr | ${XARGS} ${RMDIR})
	(cd ${STAGEDIR}${PREFIX}; ${FIND} ${PORTNAME} \
		\( -type f -or -type l \) | ${SORT} | ${SED} \
		-e 's/\/man\/man[1578]\/.*[1578]$$/&.gz/' >> ${TMPPLIST})

.include <bsd.port.mk>
