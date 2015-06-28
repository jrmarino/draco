# Created by: John Marino <marino@FreeBSD.org>
# $FreeBSD$

PORTNAME=	spark
PORTVERSION=	2015
CATEGORIES=	adacore
MASTER_SITES=	http://downloads.dragonlace.net/src/:main
DISTFILES=	${DISTNAME}.tar.bz2

MAINTAINER=	marino@FreeBSD.org
COMMENT=	Technology for engineering high-reliability s/w applications

LICENSE=	GPLv3

VALID_CATEGORIES=	adacore

BUILD_DEPENDS=	gnatcoll-gpl>=2015:${PORTSDIR}/adacore/gnatcoll \
		gnatpython>=20101207:${PORTSDIR}/devel/gnatpython \
		gprbuild:${PORTSDIR}/devel/gprbuild
RUN_DEPENDS=	gnatwhy3:${PORTSDIR}/math/why3-gpl \
		alt-ergo-spark:${PORTSDIR}/math/alt-ergo-spark

USES=		adacore gmake python:build
ALL_TARGET=	gnat2why gnatprove
GNATSRC=	${WRKSRC}/gnat2why/gnat_src
GG=		gcc-4.9-gpl-2015-src
DISTFILES+=	${GG}.tar.bz2

MAKE_JOBS_UNSAFE= yes

post-extract:
	(cd ${WRKDIR} && ${TAR} -xf ${DISTDIR}/${GG}.tar.bz2 ${GG}/gcc/ada)
	${LN} -s ${WRKDIR}/${GG}/gcc/ada ${GNATSRC}

post-patch:
	@${REINPLACE_CMD} -e 's|/usr/bin/env python|${PYTHON_CMD}|' \
		${WRKSRC}/testsuite/gnatprove/update-expected-outputs \
		${WRKSRC}/testsuite/gnatprove/run-tests \
		${WRKSRC}/testsuite/update-fixedbugs-outputs \
		${WRKSRC}/testsuite/gnatmerge/run-tests \
		${WRKSRC}/trace_units_script/Trace_Units.py
	@${REINPLACE_CMD} -e 's|"alt-ergo"|"alt-ergo-spark"|' \
		${WRKSRC}/libs/containers/why/*/why3session.xml \
		${WRKSRC}/testsuite/*/lib/python/test_support.py
	@${REINPLACE_CMD} -e 's|"alt-ergo|"alt-ergo-spark|' \
		${WRKSRC}/gnatprove/gnatprove.adb

post-install:
	(cd ${WRKSRC}/install/bin && \
		${INSTALL_PROGRAM} gnat* ${STAGEDIR}${PREFIX}/bin && \
		${INSTALL_PROGRAM} spark_report ${STAGEDIR}${PREFIX}/bin)
	(cd ${WRKSRC}/install/share && \
		${COPYTREE_SHARE} . ${STAGEDIR}${PREFIX}/share)
	${INSTALL_DATA} ${WRKSRC}/install/bin/target.atp ${STAGEDIR}${DATADIR}

testsuite:
	#${MKDIR} ${WRKSRC}/testsuite/gnatprove/lib/python/out/results
	(cd ${WRKSRC}/testsuite/gnatprove && ./run-tests)

.include <bsd.port.mk>
