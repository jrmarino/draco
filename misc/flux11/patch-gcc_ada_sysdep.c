--- gcc/ada/sysdep.c.orig	2021-10-04 02:47:55 UTC
+++ gcc/ada/sysdep.c
@@ -320,6 +320,7 @@ __gnat_ttyname (int filedes ATTRIBUTE_UN
   || (defined (__svr4__) && defined (__i386__)) || defined (__Lynx__) \
   || defined (__CYGWIN__) || defined (__FreeBSD__) || defined (__OpenBSD__) \
   || defined (__GLIBC__) || defined (__APPLE__) || defined (__DragonFly__) \
+  || defined (__NetBSD__) \
   || defined (__QNX__)
 
 # ifdef __MINGW32__
@@ -373,6 +374,7 @@ getc_immediate_common (FILE *stream,
     || defined (_AIX) || (defined (__svr4__) && defined (__i386__)) \
     || defined (__Lynx__) || defined (__FreeBSD__) || defined (__OpenBSD__) \
     || defined (__GLIBC__) || defined (__APPLE__) || defined (__DragonFly__) \
+    || defined (__NetBSD__) \
     || defined (__QNX__)
   char c;
   int nread;
@@ -394,6 +396,7 @@ getc_immediate_common (FILE *stream,
     || defined (_AIX) || (defined (__svr4__) && defined (__i386__)) \
     || defined (__Lynx__) || defined (__FreeBSD__) || defined (__OpenBSD__) \
     || defined (__GLIBC__) || defined (__APPLE__) || defined (__DragonFly__) \
+    || defined (__NetBSD__) \
     || defined (__QNX__)
       eof_ch = termios_rec.c_cc[VEOF];
 
@@ -831,6 +834,7 @@ __gnat_localtime_tzoff (const time_t *ti
 
 #elif defined (__APPLE__) || defined (__FreeBSD__) || defined (__linux__) \
   || defined (__GLIBC__) || defined (__DragonFly__) || defined (__OpenBSD__) \
+  || defined (__NetBSD__) \
   || defined (__DJGPP__) || defined (__QNX__)
 {
   localtime_r (timer, &tp);
