# $NetBSD: buildlink3.mk,v 1.1.1.1 2011/02/03 18:50:53 drochner Exp $

BUILDLINK_TREE+=	xmlada

.if !defined(XMLADA_BUILDLINK3_MK)
XMLADA_BUILDLINK3_MK:=

BUILDLINK_API_DEPENDS.xmlada+=	xmlada>=3.2.1
BUILDLINK_PKGSRCDIR.xmlada?=	../../textproc/xmlada

.include "../../lang/gnat-aux/buildlink3.mk"
.endif	# XMLADA_BUILDLINK3_MK

BUILDLINK_TREE+=	-xmlada
