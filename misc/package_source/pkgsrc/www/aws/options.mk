# $NetBSD$


# xmlada is built-in (not optional) due to gprbuild dependency
# zlib is built-in because it's mandatory.  It even comes with AWS.
# ASIS is not supported right now (custom build, requires part of GNAT)

PKG_OPTIONS_VAR=	PKG_OPTIONS.aws
PKG_SUPPORTED_OPTIONS=	demos ssl ldap ipv6 debug disable-shared-rt relocatable
PKG_SUGGESTED_OPTIONS=	demos ssl


.include "../../mk/bsd.prefs.mk"
.include "../../mk/bsd.options.mk"

CONFIGURE_ARGS+=	CJOBS=1
CONFIGURE_ARGS+=	PYTHON=python2.6
DOTBUILD=		release
BUILD_DEMOS=
DEMO_DIRS=

###################
##  SSL Support  ##
###################

.if !empty(PKG_OPTIONS:Mssl)
.include "../../security/openssl/buildlink3.mk"
CONFIGURE_ARGS+= SOCKET=openssl
.endif



####################
##  LDAP Support  ##
####################

.if !empty(PKG_OPTIONS:Mldap)
CONFIGURE_ARGS+= LDAP=true
DEPENDS+= openldap>=2.4:/../../databases/openldap
.endif



#####################
##  DEMOS Support  ##
#####################

.if !empty(PKG_OPTIONS:Mdemos)
BUILD_DEMOS=	YES
#DEMO_DIRS+=	agent
DEMO_DIRS+=	auth
DEMO_DIRS+=	com
DEMO_DIRS+=	dispatch
DEMO_DIRS+=	hello_world
#DEMO_DIRS+=	hello_wsdl
DEMO_DIRS+=	hotplug
#DEMO_DIRS+=	interoplab
DEMO_DIRS+=	jabber_demo
DEMO_DIRS+=	multiple_sessions
#DEMO_DIRS+=	res_demo
DEMO_DIRS+=	runme
DEMO_DIRS+=	soap_demo
DEMO_DIRS+=	soap_disp
DEMO_DIRS+=	soap_vs
DEMO_DIRS+=	split
#DEMO_DIRS+=	test_ldap
DEMO_DIRS+=	test_mail
DEMO_DIRS+=	text_input
DEMO_DIRS+=	vh_demo
DEMO_DIRS+=	web_block
DEMO_DIRS+=	web_block_ajax
#DEMO_DIRS+=	web_block_ajax_templates
DEMO_DIRS+=	web_elements
DEMO_DIRS+=	web_mail
DEMO_DIRS+=	wps
DEMO_DIRS+=	ws
DEMO_DIRS+=	zdemo
.endif



####################
##  IPv6 Support  ##
####################

.if !empty(PKG_OPTIONS:Mipv6)
CONFIGURE_ARGS+= IPv6=true
.endif



#####################
##  Debug Support  ##
#####################

.if !empty(PKG_OPTIONS:Mdebug)
CONFIGURE_ARGS+= DEBUG=true
DOTBUILD=debug
.endif



##############################
##  Shared Runtime Library  ##
##############################

.if !empty(PKG_OPTIONS:Mdisable-shared-rt)
CONFIGURE_ARGS+= ENABLE_SHARED=false
.endif



############################
##  Default Library Type  ##
############################

.if !empty(PKG_OPTIONS:Mrelocatable)
CONFIGURE_ARGS+= DEFAULT_LIBRARY_TYPE=relocatable
.endif
