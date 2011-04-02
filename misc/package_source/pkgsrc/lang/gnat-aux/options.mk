# $NetBSD$

PKG_OPTIONS_VAR=	PKG_OPTIONS.gnat-aux
PKG_SUPPORTED_OPTIONS=	ada cxx testc testlto
PKG_SUGGESTED_OPTIONS=	ada cxx

.include "../../mk/bsd.options.mk"


#########################
##  ADD LANGUAGE: Ada  ##
#########################

.if !empty(PKG_OPTIONS:Mada)
LANGS+= ada
.endif


#########################
##  ADD LANGUAGE: C++  ##
#########################

.if !empty(PKG_OPTIONS:Mcxx)
LANGS+= c++
.endif
