# New ports collection makefile for:  aws
# Date created:                       12 December 2010
# Whom:                               John Marino <draco@marino.st>
#
# $FreeBSD$
#

PORTNAME=	aws
PORTVERSION=	2.10.0
CATEGORIES=	www
MASTER_SITES=	http://downloads.dragonlace.net/src/ \
		http://dragonlace.mirrors.ada.cx/src/

MAINTAINER=	draco@marino.st
COMMENT=	Adacore Ada Web Server and framework

BUILD_DEPENDS=	gnat-aux>=20101204:${PORTSDIR}/lang/gnat-aux
BUILD_DEPENDS+= gprbuild-aux>=20101120:${PORTSDIR}/devel/gprbuild-aux
BUILD_DEPENDS+= python26>=2.6:${PORTSDIR}/lang/python26

USE_BZIP2=	yes
GNU_CONFIGURE=	yes
USE_GMAKE=	yes
CC=		gnatgcc
DOTBUILD=	release
MAKE_ENV+=	ADA_PROJECT_PATH=${PREFIX}/lib/gnat

OPTIONS=	SSL   "Enable HTTPS/SSL support" on
OPTIONS+=	IPV6  "Enable IPv6 support" off
OPTIONS+=	DEMOS "Build demo programs in examples/aws/demos" off
OPTIONS+=	DEBUG "Build AWS with debugging symbols" off
OPTIONS+=	LDAP  "Enable LDAP support" off
OPTIONS+=	DSRT  "Disable shared runtime" off
OPTIONS+=	RELOC "Build shared libraries (default is static)" off

CONF_ARGS= 	CJOBS=1
CONF_ARGS+=	PYTHON=python2.6

.include <bsd.port.options.mk>

###################
##  SSL Support  ##
###################

.if defined(WITH_SSL)
CONF_ARGS+=	SOCKET=openssl
BUILD_DEPENDS+= openssl>=1:${PORTSDIR}/security/openssl
RUN_DEPENDS+=	openssl>=1:${PORTSDIR}/security/openssl
.endif

####################
##  LDAP Support  ##
####################

.if defined(WITH_LDAP)
CONF_ARGS+=	LDAP=true
RUN_DEPENDS+=	openldap24>=2.4:${PORTSDIR}/net/openldap24-client
.endif

####################
##  IPv6 Support  ##
####################

.if defined(WITH_IPV6)
CONF_ARGS+=	IPv6=true
.endif

#####################
##  Debug Support  ##
#####################

.if defined(WITH_DEBUG)
CONF_ARGS+=	DEBUG=true
.endif

##############################
##  Shared Runtime Library  ##
##############################

.if defined(WITH_DSRT)
CONF_ARGS+=	ENABLE_SHARED=false
.endif

############################
##  Default Library Type  ##
############################

.if defined(WITH_RELOC)
CONF_ARGS+=	DEFAULT_LIBRARY_TYPE=relocatable
.endif

#####################
##  DEMOS Support  ##
#####################

.if defined(WITH_DEMOS)
DEMO_DIRS+=	agent
DEMO_DIRS+=	auth
DEMO_DIRS+=	com
DEMO_DIRS+=	dispatch
DEMO_DIRS+=	hello_world
DEMO_DIRS_MK+=	hello_wsdl
DEMO_DIRS+=	hotplug
DEMO_DIRS_MK+=	interoplab
DEMO_DIRS+=	jabber_demo
DEMO_DIRS+=	multiple_sessions
DEMO_DIRS_MK+=	res_demo
DEMO_DIRS+=	runme
DEMO_DIRS+=	soap_demo
DEMO_DIRS+=	soap_disp
DEMO_DIRS+=	soap_vs
DEMO_DIRS+=	split
DEMO_DIRS+=	test_mail
DEMO_DIRS+=	text_input
DEMO_DIRS+=	vh_demo
DEMO_DIRS+=	web_block
DEMO_DIRS+=	web_block_ajax
DEMO_DIRS_MK+=	web_block_ajax_templates
DEMO_DIRS+=	web_elements
DEMO_DIRS+=	web_mail
DEMO_DIRS+=	wps
DEMO_DIRS+=	ws
DEMO_DIRS+=	zdemo

.if defined (WITH_LDAP)
DEMO_DIRS+=	test_ldap
PLIST_SUB+=	LDAP=""
.else
PLIST_SUB+=	LDAP="@comment "
.endif
PLIST_SUB+=	DEMO=""
.else
PLIST_SUB+=	DEMO="@comment "
PLIST_SUB+=	LDAP="@comment "
.endif

pre-configure:
	gprconfig --batch --config=Ada --config=C --config="Project File"

do-configure:
	cd ${WRKSRC} && ${SETENV} ${CONFIGURE_ENV} \
	${GMAKE} setup ${CONF_ARGS}

do-build:
	cd ${WRKSRC} && ${SETENV} ${MAKE_ENV} ${GMAKE} build build_doc

test:
	cd ${WRKSRC}/regtests && python2.6 testsuite.py

do-install:
	cd ${WRKSRC} && ${GMAKE} install
.if defined(WITH_DEMOS)
	${MAKE} demos
	${MAKE} demos-install
.endif

demos:
.for dd in ${DEMO_DIRS}
	cd ${WRKSRC}/demos/${dd} && gnatmake -P ${dd}.gpr
.endfor
.for dd in ${DEMO_DIRS_MK}
	cd ${WRKSRC}/demos/${dd} && ${GMAKE}
.endfor

demos-install:
	${MKDIR} ${PREFIX}/share/examples/aws/demos
.for dd in ${DEMO_DIRS}
	${FIND} ${WRKSRC}/demos/${dd} -type f -perm -0001 \
	-exec ${INSTALL} {} ${PREFIX}/share/examples/aws/demos/ \;
.endfor
.for dd in ${DEMO_DIRS_MK}
	${FIND} ${WRKSRC}/demos/${dd} -type f -perm -0001 \
	-exec ${INSTALL} {} ${PREFIX}/share/examples/aws/demos/ \;
.endfor

.include <bsd.port.mk>
