--- libgfortran/acinclude.m4.orig	2013-05-24 04:23:33.000000000 -0500
+++ libgfortran/acinclude.m4
@@ -100,7 +100,7 @@ void foo (void);
 	      [Define to 1 if the target supports #pragma weak])
   fi
   case "$host" in
-    *-*-darwin* | *-*-hpux* | *-*-cygwin* | *-*-mingw* )
+    *-*-darwin* | *-*-hpux* | *-*-cygwin* | *-*-mingw* | *-*-netbsd* )
       AC_DEFINE(GTHREAD_USE_WEAK, 0,
 		[Define to 0 if the target shouldn't use #pragma weak])
       ;;
