# $NetBSD$

BUILDLINK_TREE+=	xmlada

.if !defined(XMLADA_BUILDLINK3_MK)
XMLADA_BUILDLINK3_MK:=

BUILDLINK_API_DEPENDS.xmlada+=	xmlada>=3.2.1
BUILDLINK_PKGSRCDIR.xmlada?=	../../textproc/xmlada

.include "../../lang/gnat-aux/buildlink3.mk"
.endif	# XMLADA_BUILDLINK3_MK

BUILDLINK_TREE+=	-xmlada
