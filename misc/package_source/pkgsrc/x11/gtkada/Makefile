# $NetBSD$
#

DISTNAME=	gtkada-2.22
CATEGORIES=	x11
MASTER_SITES=	http://downloads.dragonlace.net/src/ \
		http://dragonlace.mirrors.ada.cx/src/
EXTRACT_SUFX=	.tar.bz2

MAINTAINER=	draco@marino.st
HOMEPAGE=	http://libre.adacore.com/libre/tools/gtkada/
COMMENT=	Ada graphical toolkit based on Gtk+
LICENSE=	gnu-gpl-v2

PKG_DESTDIR_SUPPORT=	user-destdir

GNU_CONFIGURE=		yes
USE_TOOLS+=		gmake pkg-config

FULL_PATH=		/sbin:/bin:/usr/sbin:/usr/bin:${PREFIX}/sbin:${PREFIX}/bin:${PREFIX}/lib/pkgconfig
MY_CONFIGURE_ENV+=	PATH=${FULL_PATH}
PKGCONFIG_OVERRIDE+=	projects/gtkada.pc.in


GENERATE_PLIST=  cd ${PREFIX};
GENERATE_PLIST+= ${FIND} bin/gtkada-config;
GENERATE_PLIST+= ${FIND} bin/gtkada-dialog;
GENERATE_PLIST+= ${FIND} lib/gnat/gtkada.gpr;
GENERATE_PLIST+= ${FIND} lib/pkgconfig/gtkada.pc;
GENERATE_PLIST+= ${FIND} share/gps/plug-ins/gtkada.xml;
GENERATE_PLIST+= ${FIND} share/examples/gtkada/ -name \*    -type f | ${SORT} -dr;
GENERATE_PLIST+= ${FIND} share/doc/gtkada/      -name \*    -type f | ${SORT} -dr;
GENERATE_PLIST+= ${FIND} include/gtkada/        -name \*    -type f | ${SORT};
GENERATE_PLIST+= ${FIND} lib/gnat/gtkada/       -name \*    -type f | ${SORT};
GENERATE_PLIST+= ${FIND} lib/gtkada/            -name \* \( -type f -or -type l \) | ${SORT} -dr;


post-wrapper:
	# We disable at least the wrapper binaries on all GNAT AUX ports to
	# avoid linkage and build problems.  In this case we need the wrappers
	# for the sed transformation script.
	${RM} ${WRKDIR}/.wrapper/bin/*


pre-configure:
	# explicitly request GNAT-AUX compiler
	# It is done here to fool tough pkgsrc compiler checks
	${SETENV} CC=gnatgcc


do-configure:
	cd ${WRKSRC} && ${SETENV} ${MY_CONFIGURE_ENV} ./configure ${CONFIGURE_ARGS}


post-install:
	${TOUCH} ${PREFIX}/lib/gnat/gtkada/IGNORE.ME
	${TOUCH} ${PREFIX}/share/doc/gtkada/gtkada_rm/IGNORE.ME


# gtk2 includes atk, glib and pango among others
.include "../../x11/gtk2/buildlink3.mk"
.include "../../lang/gnat-aux/buildlink3.mk"
.include "../../mk/bsd.pkg.mk"
