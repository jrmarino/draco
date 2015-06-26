# Created by: John Marino <marino@FreeBSD.org>
# $FreeBSD$

PORTNAME=	gnat_util
PORTVERSION=	2015
CATEGORIES=	adacore
MASTER_SITES=	http://downloads.dragonlace.net/src/
PKGNAMESUFFIX=	-gpl
DISTNAME=	gnat_util-gpl-${PORTVERSION}-src

MAINTAINER=	marino@FreeBSD.org
COMMENT=	Library used to build GNAT tools such as ASIS

LICENSE=	GPLv3 GPLv3RLE
LICENSE_COMB=	multi

VALID_CATEGORIES=	adacore

CONFLICTS_INSTALL=	gnat_util-*

BUILD_DEPENDS=	gprbuild>=20120510:${PORTSDIR}/devel/gprbuild

USES=		adacore gmake

post-extract:
	@${REINPLACE_CMD} -e 's|(prefix)|(DESTDIR)$$(prefix)|g' \
		${WRKSRC}/Makefile

.include <bsd.port.mk>
