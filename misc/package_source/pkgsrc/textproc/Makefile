# $NetBSD$
#

DISTNAME=	xmlada-gpl-${XMLADA_VERS}-src
PKGNAME=	xmlada-${XMLADA_VERS}
CATEGORIES=	textproc
MASTER_SITES=	http://downloads.dragonlace.net/src/ \
		http://dragonlace.mirrors.ada.cx/src/
EXTRACT_SUFX=	.tgz

MAINTAINER=	draco@marino.st
HOMEPAGE=	http://libre.adacore.com/libre/tools/xmlada/
COMMENT=	Adacore XML suite for the Ada language
LICENSE=	gnu-gpl-v2

PKG_DESTDIR_SUPPORT=	user-destdir
XMLADA_VERS=		3.2.1
WRKSRC=			${WRKDIR}/xmlada-${XMLADA_VERS}-src
GNU_CONFIGURE=		yes
USE_LANGUAGES=		c # ada is not valid enumeration
USE_TOOLS=		gmake



GENERATE_PLIST=  cd ${PREFIX};
GENERATE_PLIST+= ${FIND} bin/xmlada-config;
GENERATE_PLIST+= ${FIND} lib/gnat/*.gpr	          -type f | ${SORT};
GENERATE_PLIST+= ${FIND} lib/gnat/xmlada/*.lgpr   -type f | ${SORT};
GENERATE_PLIST+= ${FIND} lib/xmlada/relocatable/* -type f | ${SORT};
GENERATE_PLIST+= ${FIND} lib/xmlada/static/*      -type f | ${SORT};
GENERATE_PLIST+= ${FIND} include/xmlada/*         -type f | ${SORT};
GENERATE_PLIST+= ${FIND} share/doc/xmlada/xml.*   -type f | ${SORT};
GENERATE_PLIST+= ${FIND} share/gps/plug-ins/xmlada_gps.py;



# no special configuration options necessary
pre-configure:
	# explicitly request GNAT-AUX compiler
	# It is done here to fool tough pkgscr compiler checks
	${SETENV} CC=gnatgcc



.include "../../lang/gnat-aux/buildlink3.mk"
.include "../../mk/bsd.pkg.mk"
