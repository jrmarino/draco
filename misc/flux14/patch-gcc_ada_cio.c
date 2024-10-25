--- gcc/ada/cio.c.orig	2021-08-18 01:40:08 UTC
+++ gcc/ada/cio.c
@@ -48,7 +48,7 @@ extern "C" {
 
 /* Don't use macros on GNU/Linux since they cause incompatible changes between
    glibc 2.0 and 2.1 */
-#ifdef __linux__
+#if defined __linux__ && !defined __ANDROID__
 #undef putchar
 #undef getchar
 #undef fputc
