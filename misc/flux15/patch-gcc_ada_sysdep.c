--- gcc/ada/sysdep.c.orig
+++ gcc/ada/sysdep.c
@@ -338,6 +338,7 @@ __gnat_ttyname (int filedes ATTRIBUTE_UNUSED)
   || (defined (__svr4__) && defined (__i386__)) || defined (__Lynx__) \
   || defined (__CYGWIN__) || defined (__FreeBSD__) || defined (__OpenBSD__) \
   || defined (__GLIBC__) || defined (__APPLE__) || defined (__DragonFly__) \
+  || defined (__NetBSD__) \
   || defined (__QNX__)
 
 # ifdef __MINGW32__
@@ -387,6 +388,7 @@ getc_immediate_common (FILE *stream,
     || defined (_AIX) || (defined (__svr4__) && defined (__i386__)) \
     || defined (__Lynx__) || defined (__FreeBSD__) || defined (__OpenBSD__) \
     || defined (__GLIBC__) || defined (__APPLE__) || defined (__DragonFly__) \
+    || defined (__NetBSD__) \
     || defined (__QNX__)
   char c;
   int nread;
@@ -408,6 +410,7 @@ getc_immediate_common (FILE *stream,
     || defined (_AIX) || (defined (__svr4__) && defined (__i386__)) \
     || defined (__Lynx__) || defined (__FreeBSD__) || defined (__OpenBSD__) \
     || defined (__GLIBC__) || defined (__APPLE__) || defined (__DragonFly__) \
+    || defined (__NetBSD__) \
     || defined (__QNX__)
       eof_ch = termios_rec.c_cc[VEOF];
 
@@ -847,6 +850,7 @@ __gnat_localtime_tzoff (const OS_Time *timer ATTRIBUTE
 
 #elif defined (__APPLE__) || defined (__FreeBSD__) || defined (__linux__) \
   || defined (__GLIBC__) || defined (__DragonFly__) || defined (__OpenBSD__) \
+  || defined (__NetBSD__) \
   || defined (__DJGPP__) || defined (__QNX__)
 {
   localtime_r (&time, &tp);
