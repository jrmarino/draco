# $NetBSD$
#

DISTNAME=	gprbuild-aux-${SNAPSHOT}
CATEGORIES=	devel
MASTER_SITES=	http://downloads.dragonlace.net/src/ \
		http://dragonlace.mirrors.ada.cx/src/
EXTRACT_SUFX=	.tar.bz2

MAINTAINER=	draco@marino.st
HOMEPAGE=	http://www.adacore.com/home/products/gnatpro/toolsuite/gprbuild/
COMMENT=	Adacore multi-language software build tool
LICENSE=	gnu-gpl-v2

PKG_DESTDIR_SUPPORT=	user-destdir
SNAPSHOT=		20101120
GNU_CONFIGURE=		yes
USE_LANGUAGES=		c # Ada is not option.
USE_TOOLS=		gmake





# Note: share/examples/gprbuild has several empty directories and due to this
# the automatic pruning doesn't work during the deinstall phase.  There is no
# post-deinstall target either so it can't be cleaned up via brute force.
# Looks like a weakness of pkgsrc to not recognize this likely common case.
GENERATE_PLIST=  cd ${PREFIX};
GENERATE_PLIST+= ${FIND} bin/gprbuild;
GENERATE_PLIST+= ${FIND} bin/gprclean;
GENERATE_PLIST+= ${FIND} bin/gprconfig;
GENERATE_PLIST+= ${FIND} share/gprconfig/*.xml    -type f | ${SORT};
GENERATE_PLIST+= ${FIND} share/examples/gprbuild/ -name \* \( -type f -or -type l \) | ${SORT} -dr;
GENERATE_PLIST+= ${FIND} share/doc/gprbuild/      -name \*    -type f | ${SORT} -dr;
GENERATE_PLIST+= ${FIND} libexec/gprbuild/        -name gpr\* -type f | ${SORT} -dr;



# no special configuration options necessary
pre-configure:
	# explicitly request GNAT-AUX compiler (required by XML/Ada too)
	# It is done here to fool tough pkgsrc compiler checks
	${SETENV} CC=gnatgcc


do-wrapper:
	# GPRbuild has a linkage problem while using standard wrappers
	${ECHO} "##  Wrappers are intentionally disabled  ##"



.include "../../textproc/xmlada/buildlink3.mk"
.include "../../mk/bsd.pkg.mk"
