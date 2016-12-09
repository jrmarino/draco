--- gcc/ada/sysdep.c.orig	2016-12-09 23:16:55 UTC
+++ gcc/ada/sysdep.c
@@ -287,6 +287,7 @@ __gnat_ttyname (int filedes)
   || defined (__MACHTEN__) || defined (__hpux__) || defined (_AIX) \
   || (defined (__svr4__) && defined (__i386__)) || defined (__Lynx__) \
   || defined (__CYGWIN__) || defined (__FreeBSD__) || defined (__OpenBSD__) \
+  || defined (__NetBSD__) \
   || defined (__GLIBC__) || defined (__APPLE__) || defined (__DragonFly__)
 
 # ifdef __MINGW32__
@@ -339,6 +340,7 @@ getc_immediate_common (FILE *stream,
     || defined (__CYGWIN32__) || defined (__MACHTEN__) || defined (__hpux__) \
     || defined (_AIX) || (defined (__svr4__) && defined (__i386__)) \
     || defined (__Lynx__) || defined (__FreeBSD__) || defined (__OpenBSD__) \
+    || defined (__NetBSD__) \
     || defined (__GLIBC__) || defined (__APPLE__) || defined (__DragonFly__)
   char c;
   int nread;
@@ -359,6 +361,7 @@ getc_immediate_common (FILE *stream,
     || defined (__MACHTEN__) || defined (__hpux__) \
     || defined (_AIX) || (defined (__svr4__) && defined (__i386__)) \
     || defined (__Lynx__) || defined (__FreeBSD__) || defined (__OpenBSD__) \
+    || defined (__NetBSD__) \
     || defined (__GLIBC__) || defined (__APPLE__) || defined (__DragonFly__)
       eof_ch = termios_rec.c_cc[VEOF];
 
@@ -795,6 +798,7 @@ __gnat_localtime_tzoff (const time_t *ti
    struct tm */
 
 #elif defined (__APPLE__) || defined (__FreeBSD__) || defined (__linux__) \
+  || defined (__NetBSD__) \
   || defined (__GLIBC__) || defined (__DragonFly__) || defined (__OpenBSD__)
 {
   localtime_r (timer, &tp);
