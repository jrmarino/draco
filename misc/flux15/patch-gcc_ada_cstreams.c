--- gcc/ada/cstreams.c.orig	2021-10-04 02:20:31 UTC
+++ gcc/ada/cstreams.c
@@ -70,7 +70,7 @@
 extern "C" {
 #endif
 
-#ifdef __linux__
+#if defined __linux__ && !defined __ANDROID__
 /* Don't use macros on GNU/Linux since they cause incompatible changes between
    glibc 2.0 and 2.1 */
 
@@ -193,7 +193,7 @@ __gnat_full_name (char *nam, char *buffe
 	  *p = '\\';
     }
 
-#elif defined (__FreeBSD__) || defined (__DragonFly__) || defined (__OpenBSD__)
+#elif defined (__FreeBSD__) || defined (__DragonFly__) || defined (__OpenBSD__) || defined (__NetBSD__)
 
   /* Use realpath function which resolves links and references to . and ..
      on those Unix systems that support it. Note that GNU/Linux provides it but
@@ -275,7 +275,7 @@ __gnat_fseek64 (FILE *stream, __int64 of
 }
 
 #elif defined (__linux__) || defined (__sun__) || defined (__FreeBSD__) \
-  || defined (__APPLE__)
+  || defined (__APPLE__) || defined (__NetBSD__)
 /* section for platforms having ftello/fseeko */
 
 __int64
