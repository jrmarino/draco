# $NetBSD$
#

DISTNAME=	aws-gpl-${AWS_VERSION}-src
CATEGORIES=	www
MASTER_SITES=	http://downloads.dragonlace.net/src/
EXTRACT_SUFX=	.tgz

MAINTAINER=	draco@marino.st
HOMEPAGE=	http://libre.adacore.com/libre/tools/aws/
COMMENT=	Adacore Ada Web Server and framework
LICENSE=	gnu-gpl-v2

PKG_DESTDIR_SUPPORT=	user-destdir
AWS_VERSION=		2.8.0
USE_TOOLS+=		gmake

.include "options.mk"


MAKE_ENV+=	ADA_PROJECT_PATH=${PREFIX}/lib/gnat

GENERATE_PLIST=  cd ${PREFIX};
GENERATE_PLIST+= ${FIND} bin/awsres;
GENERATE_PLIST+= ${FIND} bin/aws_password;
GENERATE_PLIST+= ${FIND} lib/gnat/aws.gpr;
GENERATE_PLIST+= ${FIND} share/examples/aws/ -name \*    -type f | ${SORT} -dr;
GENERATE_PLIST+= ${FIND} share/doc/aws/      -name \*    -type f | ${SORT};
GENERATE_PLIST+= ${FIND} lib/gnat/aws/       -name \*    -type f | ${SORT};
GENERATE_PLIST+= ${FIND} lib/aws/            -name \*    -type f | ${SORT} -dr;
GENERATE_PLIST+= ${FIND} include/aws/        -name \*    -type f | ${SORT} -dr;
GENERATE_PLIST+= ${FIND} share/gps/plug-ins/ -name aws\* -type f | ${SORT};


post-wrapper:
	# We disable at least the wrapper binaries on all GNAT AUX to
	# avoid potential linkage problems
	${RM} ${WRKDIR}/.wrapper/bin/*



pre-configure:
	gprconfig --batch --config=Ada --config=C --config="Project File"



do-configure:
	# explicitly request GNAT-AUX compiler
	# It is done here to fool tough pkgsrc compiler checks
	${SETENV} CC=gnatgcc
	cd ${WRKSRC} && ${SETENV} ${CONFIGURE_ENV} \
	${GMAKE} setup ${CONFIGURE_ARGS}



do-build:
	cd ${WRKSRC} && ${SETENV} ${MAKE_ENV} ${GMAKE} build



do-install:
	cd ${WRKSRC} && ${GMAKE} install;
	${TOUCH} ${PREFIX}/share/examples/aws/bin/IGNORE.ME
.if !empty(PKG_OPTIONS:Mdemos)
.if !empty(PKG_OPTIONS:Mrelocatable)
	${MKDIR} -p ${PREFIX}/share/examples/aws/reloc_demos
	${INSTALL} ${WRKSRC}/.build/native/${DOTBUILD}/relocatable/demos/* \
	      ${PREFIX}/share/examples/aws/reloc_demos/
.else
	${MKDIR} -p ${PREFIX}/share/examples/aws/static_demos
	${INSTALL} ${WRKSRC}/.build/native/${DOTBUILD}/static/demos/* \
	      ${PREFIX}/share/examples/aws/static_demos/
.endif
.endif


.include "../../devel/zlib/buildlink3.mk"
.include "../../devel/gprbuild-aux/buildlink3.mk"
.include "../../mk/bsd.pkg.mk"
