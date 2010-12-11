# $NetBSD$
#

DISTNAME=	gnatpython-${SNAPSHOT}
CATEGORIES=	devel
MASTER_SITES=	http://downloads.dragonlace.net/src/ \
		http://dragonlace.mirrors.ada.cx/src/
EXTRACT_SUFX=	.tar.bz2

MAINTAINER= 	draco@marino.st
HOMEPAGE=	http://forge.open-do.org/projects/gnatpython
COMMENT=	Python package used in Ada testsuites
LICENSE= 	gnu-gpl-v3
SNAPSHOT=	20101207

PKG_DESTDIR_SUPPORT=	user-destdir

do-build:
	cd ${WRKSRC} && python2.6 ./setup.py build


do-install:
	cd ${WRKSRC} && python2.6 ./setup.py install


.include "../../lang/python26/buildlink3.mk"
.include "../../mk/bsd.pkg.mk"
