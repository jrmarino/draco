# $NetBSD$


# xmlada is built-in (not optional) due to gprbuild dependency
# zlib is built-in because it's mandatory.  It even comes with AWS.
# ASIS is not supported right now (custom build, requires part of GNAT)

PKG_OPTIONS_VAR=	PKG_OPTIONS.aws
PKG_SUPPORTED_OPTIONS=	demos ssl ldap inet6 debug disable-shared-rt relocatable
PKG_SUGGESTED_OPTIONS=	demos


.include "../../mk/bsd.prefs.mk"
.include "../../mk/bsd.options.mk"


DOTBUILD=release

###################
##  SSL Support  ##
###################

.if !empty(PKG_OPTIONS:Mssl)
CONFIGURE_ARGS+= SOCKET=openssl
.include "../../security/openssl/buildlink3.mk"
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
CONFIGURE_ARGS+= DEMOS=true
.endif



####################
##  IPv6 Support  ##
####################

.if !empty(PKG_OPTIONS:Minet6)
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
