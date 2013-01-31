# New ports collection makefile for:  xmlada
# Date created:                       12 December 2010
# Whom:                               John Marino <draco@marino.st>
#
# $FreeBSD: ports/textproc/xmlada/Makefile,v 1.14 2011/10/12 18:20:58 pawel Exp $
#

PORTNAME=	xmlada
PORTVERSION=	4.2.0.0
CATEGORIES=	textproc
MASTER_SITES=	http://downloads.dragonlace.net/src/ \
		http://dragonlace.mirrors.ada.cx/src/

MAINTAINER=	draco@marino.st
COMMENT=	Adacore XML suite for the Ada language

BUILD_DEPENDS=	gnat-aux>=20110325:${PORTSDIR}/lang/gnat-aux

GNU_CONFIGURE=	yes
USE_GMAKE=	yes
USE_BZIP2=	yes
USE_LDCONFIG=	yes
CC=		gnatgcc

.include <bsd.port.mk>
