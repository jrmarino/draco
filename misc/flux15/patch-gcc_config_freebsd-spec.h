--- gcc/config/freebsd-spec.h.orig	2026-03-31 17:38:23 UTC
+++ gcc/config/freebsd-spec.h
@@ -68,7 +68,7 @@ see the files COPYING3 and COPYING.RUNTIME respectivel
 		       %{!p:%{profile:gcrt1.o%s} \
 			 %{!profile: \
                             %{pie: Scrt1.o%s;:crt1.o%s}}}}} \
-   crti.o%s %{static:crtbeginT.o%s;shared|pie:crtbeginS.o%s;:crtbegin.o%s}"
+   crti.o%s %{static|:crtbegin.o%s;shared|pie:crtbeginS.o%s}"
 
 /* Provide a ENDFILE_SPEC appropriate for FreeBSD.  Here we tack on
    the magical crtend.o file (see crtstuff.c) which provides part of
