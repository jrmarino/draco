# New ports collection makefile for:  gnatpython
# Date created:                       12 December 2010
# Whom:                               John Marino <draco@marino.st>
#
# $FreeBSD$
#

PORTNAME=	gnatpython
PORTVERSION=	${SNAPSHOT}
CATEGORIES=	devel
MASTER_SITES=	http://downloads.dragonlace.net/src/ \
		http://dragonlace.mirrors.ada.cx/src/

MAINTAINER=	draco@marino.st
COMMENT=	Python package used in Ada testsuites

SNAPSHOT=	20101207
BUILD_DEPENDS=	python26>=2.6:${PORTSDIR}/lang/python26
RUN_DEPENDS=	python26>=2.6:${PORTSDIR}/lang/python26
USE_BZIP2=	yes

do-build:
	cd ${WRKSRC} && python2.6 ./setup.py build

do-install:
	cd ${WRKSRC} && python2.6 ./setup.py install

.include <bsd.port.mk>
