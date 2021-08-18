--- gcc/ada/cstreams.c.orig	2021-08-18 01:37:06 UTC
+++ gcc/ada/cstreams.c
@@ -70,7 +70,7 @@
 extern "C" {
 #endif
 
-#ifdef __linux__
+#if defined __linux__ && !defined __ANDROID__
 /* Don't use macros on GNU/Linux since they cause incompatible changes between
    glibc 2.0 and 2.1 */
 
