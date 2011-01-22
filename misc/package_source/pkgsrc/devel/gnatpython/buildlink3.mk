# $NetBSD$

BUILDLINK_TREE+=	gnatpython

.if !defined(GNATPYTHON_BUILDLINK3_MK)
GNATPYTHON_BUILDLINK3_MK:=

BUILDLINK_API_DEPENDS.gnatpython+=	gnatpython>=20101122
BUILDLINK_PKGSRCDIR.gnatpython?=	../../devel/gnatpython

.include "../../lang/python26/buildlink3.mk"
.endif	# GNATPYTHON_BUILDLINK3_MK

BUILDLINK_TREE+=	-gnatpython
