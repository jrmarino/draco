--- libgfortran/acinclude.m4.orig	2016-12-05 15:54:41 UTC
+++ libgfortran/acinclude.m4
@@ -85,7 +85,7 @@ void foo (void);
 	      [Define to 1 if the target supports #pragma weak])
   fi
   case "$host" in
-    *-*-darwin* | *-*-hpux* | *-*-cygwin* | *-*-mingw* | *-*-musl* )
+    *-*-darwin* | *-*-hpux* | *-*-cygwin* | *-*-mingw* | *-*-musl* | *-*-netbsd* )
       AC_DEFINE(GTHREAD_USE_WEAK, 0,
 		[Define to 0 if the target shouldn't use #pragma weak])
       ;;
