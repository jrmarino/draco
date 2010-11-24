# $NetBSD$

BUILDLINK_TREE+=	gprbuild-aux

.if !defined(GPRBUILD_AUX_BUILDLINK3_MK)
GPRBUILD_AUX_BUILDLINK3_MK:=

BUILDLINK_API_DEPENDS.gprbuild-aux+=	gprbuild-aux>=20101120
BUILDLINK_PKGSRCDIR.gprbuild-aux?=	../../devel/gprbuild-aux

.include "../../textproc/xmlada/buildlink3.mk"
.endif	# GPRBUILD_AUX_BUILDLINK3_MK

BUILDLINK_TREE+=	-gprbuild-aux
