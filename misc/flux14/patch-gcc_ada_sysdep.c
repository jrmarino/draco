--- gcc/ada/sysdep.c.orig	2023-07-05 04:48:38 UTC
+++ gcc/ada/sysdep.c
@@ -321,6 +321,7 @@ __gnat_ttyname (int filedes ATTRIBUTE_UN
   || (defined (__svr4__) && defined (__i386__)) || defined (__Lynx__) \
   || defined (__CYGWIN__) || defined (__FreeBSD__) || defined (__OpenBSD__) \
   || defined (__GLIBC__) || defined (__APPLE__) || defined (__DragonFly__) \
+  || defined (__NetBSD__) \
   || defined (__QNX__)
 
 # ifdef __MINGW32__
@@ -370,6 +371,7 @@ getc_immediate_common (FILE *stream,
     || defined (_AIX) || (defined (__svr4__) && defined (__i386__)) \
     || defined (__Lynx__) || defined (__FreeBSD__) || defined (__OpenBSD__) \
     || defined (__GLIBC__) || defined (__APPLE__) || defined (__DragonFly__) \
+    || defined (__NetBSD__) \
     || defined (__QNX__)
   char c;
   int nread;
@@ -391,6 +393,7 @@ getc_immediate_common (FILE *stream,
     || defined (_AIX) || (defined (__svr4__) && defined (__i386__)) \
     || defined (__Lynx__) || defined (__FreeBSD__) || defined (__OpenBSD__) \
     || defined (__GLIBC__) || defined (__APPLE__) || defined (__DragonFly__) \
+    || defined (__NetBSD__) \
     || defined (__QNX__)
       eof_ch = termios_rec.c_cc[VEOF];
 
@@ -830,6 +833,7 @@ __gnat_localtime_tzoff (const OS_Time *t
 
 #elif defined (__APPLE__) || defined (__FreeBSD__) || defined (__linux__) \
   || defined (__GLIBC__) || defined (__DragonFly__) || defined (__OpenBSD__) \
+  || defined (__NetBSD__) \
   || defined (__DJGPP__) || defined (__QNX__)
 {
   localtime_r (&time, &tp);
