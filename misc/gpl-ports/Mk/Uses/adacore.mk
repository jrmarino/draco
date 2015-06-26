# $FreeBSD$
#
# Establish GNAT GPL 20XX compiler as a build dependency
# To change default compiler, define ADA_DEFAULT in make.conf to 5
#
# Feature:      adacore
# Usage:        USES=ada
# Valid ARGS:   run
#
# MAINTAINER: marino@FreeBSD.org

.if !defined(_INCLUDE_USES_ADACORE_MK)
_INCLUDE_USES_ADACORE_MK=    yes

CC= ada

. if ${adacore_ARGS:Mrun}
RUN_DEPENDS+=	${LOCALBASE}/gnat-gpl/bin/ada:${PORTSDIR}/adacore/gnat-gpl
. endif

BUILD_DEPENDS+=	${LOCALBASE}/gnat-gpl/bin/ada:${PORTSDIR}/adacore/gnat-gpl
MAKE_ENV+=	PATH=${LOCALBASE}/gnat-gpl/bin:${PATH} \
		ADA_PROJECT_PATH=${LOCALBASE}/lib/gnat
CONFIGURE_ENV+=	PATH=${LOCALBASE}/gnat-gpl/bin:${PATH} \
		ADA_PROJECT_PATH=${LOCALBASE}/lib/gnat

.endif
