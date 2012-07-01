# $NetBSD: buildlink3.mk,v 1.2 2011/12/03 07:28:18 marino Exp $

BUILDLINK_TREE+= gcc-aux

.if !defined(GCC_AUX_BUILDLINK3_MK)
GCC_AUX_BUILDLINK3_MK:=

BUILDLINK_API_DEPENDS.gcc-aux+= gcc-aux>=20120614
BUILDLINK_PKGSRCDIR.gcc-aux?= ../../lang/gcc-aux

.include "../../devel/zlib/buildlink3.mk"
.include "../../devel/gmp/buildlink3.mk"
.include "../../math/mpfr/buildlink3.mk"
.include "../../math/mpcomplex/buildlink3.mk"
.include "../../converters/libiconv/buildlink3.mk"
.endif

BUILDLINK_TREE+= -gcc-aux