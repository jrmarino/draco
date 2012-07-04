# $NetBSD: Makefile,v 1.1.1.1 2011/02/03 19:04:02 drochner Exp $
#

DISTNAME=	gprbuild-aux-${SNAPSHOT}
CATEGORIES=	devel
MASTER_SITES=	http://downloads.dragonlace.net/src/ \
		http://dragonlace.mirrors.ada.cx/src/
DISTFILES=	gprbuild-gpl-2012-src.tgz

MAINTAINER=	draco@marino.st
HOMEPAGE=	http://www.adacore.com/home/products/gnatpro/toolsuite/gprbuild/
COMMENT=	Adacore multi-language software build tool
LICENSE=	gnu-gpl-v3

PKG_DESTDIR_SUPPORT=	user-destdir

SNAPSHOT=		20120510
GNU_CONFIGURE=		yes
USE_LANGUAGES=		c ada
USE_TOOLS=		gmake
MAKE_ENV+=		ADA_PROJECT_PATH=${PREFIX}/lib/gnat
CONFIGURE_ENV+=		ADA_PROJECT_PATH=${PREFIX}/lib/gnat
WRKSRC=			${WRKDIR}/gprbuild-2012-src

.include "../../textproc/xmlada/buildlink3.mk"
.include "../../mk/bsd.pkg.mk"
