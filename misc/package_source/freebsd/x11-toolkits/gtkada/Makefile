# New ports collection makefile for:  gtkada
# Date created:                       12 December 2010
# Whom:                               John Marino <draco@marino.st>
#
# $FreeBSD: ports/x11-toolkits/gtkada/Makefile,v 1.28 2012/06/01 05:25:48 dinoex Exp $
#

PORTNAME=	gtkada
PORTVERSION=	2.22
PORTREVISION=	1
CATEGORIES=	x11-toolkits
MASTER_SITES=	http://downloads.dragonlace.net/src/ \
		http://dragonlace.mirrors.ada.cx/src/

MAINTAINER=	draco@marino.st
COMMENT=	Ada graphical toolkit based on Gtk2+

BUILD_DEPENDS=	gnat-aux>20101204:${PORTSDIR}/lang/gnat-aux
BUILD_DEPENDS+=	gtk>=2.22:${PORTSDIR}/x11-toolkits/gtk20
RUN_DEPENDS+=	gtk>=2.22:${PORTSDIR}/x11-toolkits/gtk20

GNU_CONFIGURE=	yes
USE_GMAKE=	yes
USE_BZIP2=	yes
USE_LDCONFIG=	yes
CC=		gnatgcc

CONFIGURE_ARGS+=	--disable-nls

.include <bsd.port.mk>
