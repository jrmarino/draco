# Created by: John Marino <marino@FreeBSD.org>
# $FreeBSD: head/devel/gnatcoll/Makefile 390479 2015-06-24 13:32:41Z marino $

PORTNAME=	gnatcoll
PORTVERSION=	2015
CATEGORIES=	adacore
MASTER_SITES=	http://downloads.dragonlace.net/src/
PKGNAMESUFFIX=	-gpl
DISTNAME=	${PORTNAME}-gpl-${PORTVERSION}-src

MAINTAINER=	marino@FreeBSD.org
COMMENT=	GNAT Component Collection from Adacore

LICENSE=	GPLv3

BUILD_DEPENDS=	gprbuild>=20120510:${PORTSDIR}/devel/gprbuild \
		gnat_util-gpl>=2015:${PORTSDIR}/adacore/gnat_util
LIB_DEPENDS=	libgmp.so:${PORTSDIR}/math/gmp
RUN_DEPENDS=	gnat_util-gpl>=2015:${PORTSDIR}/adacore/gnat_util

VALID_CATEGORIES=	adacore

USES=		adacore gmake pkgconfig python
PYCOMPILE=	${PREFIX}/lib/${PYTHON_VERSION}/compileall.py
GNU_CONFIGURE=	yes

MAKE_ARGS=	PROCESSORS=${MAKE_JOBS_NUMBER} \
		Gnatcoll_Build=Production

OPTIONS_DEFINE=		READLINE SYSLOG PGSQL ICONV EXAMPLES
OPTIONS_DEFAULT=	READLINE SYSLOG
OPTIONS_SUB=		yes
SYSLOG_CONFIGURE_ENABLE=	syslog

CONFIGURE_TARGET=	${ARCH:S/amd64/x86_64/}-aux-${OPSYS:tl}${OSREL}
CONFIGURE_ARGS=		--with-gmp=${PREFIX} \
			--with-sqlite=embedded \
			--with-python=${PREFIX} \
			--without-gtk \
			--enable-pygobject \
			--disable-shared \
			--disable-pygtk

.include <bsd.port.options.mk>

.if ${PORT_OPTIONS:MREADLINE}
USES+=			readline:port
CONFIGURE_ARGS+=	--enable-gpl
.else
CONFIGURE_ARGS+=	--disable-readline
.endif

.if ${PORT_OPTIONS:MPGSQL}
USES+=			pgsql
CONFIGURE_ARGS+=	--with-postgresql=${PREFIX}
.else
CONFIGURE_ARGS+=	--without-postgresql
.endif

# There might be something wrong with iconv support as seen in GPS crash.
# That's why this option is not enabled by default
.if ${PORT_OPTIONS:MICONV}
USES+=			iconv
CONFIGURE_ARGS+=	--with-iconv=${ICONV_PREFIX}
.else
CONFIGURE_ARGS+=	--without-iconv
.endif

post-patch:
	@${REINPLACE_CMD} -e "s|@PREFIX@|${PREFIX}|g" \
	    ${WRKSRC}/src/gnatcoll_readline.gpr.in
	${MV} ${WRKSRC}/docs/_build ${WRKSRC}/docs/_hide_build

do-build:
	cd ${WRKSRC} && ${SETENV} ${MAKE_ENV} \
		${MAKE_CMD} ${MAKE_ARGS} ${ALL_TARGET}

post-install:
	${RM} -r ${STAGEDIR}${DOCSDIR}
	${RM} -r ${STAGEDIR}${PREFIX}/share/gps
	${RM} -r ${STAGEDIR}${EXAMPLESDIR}

.include <bsd.port.mk>
