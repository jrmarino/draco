--- gcc/ada/s-oscons-tmplt.c.orig	2023-07-05 04:46:10 UTC
+++ gcc/ada/s-oscons-tmplt.c
@@ -407,7 +407,7 @@ CND(FNDELAY, "Nonblocking")
 
 /* ioctl(2) requests are "int" in UNIX, but "unsigned long" on FreeBSD */
 
-#if defined (__FreeBSD__) || defined (__DragonFly__)
+#if defined (__FreeBSD__) || defined (__DragonFly__) || defined (__NetBSD__)
 # define CNI CNU
 # define IOCTL_Req_T "Interfaces.C.unsigned"
 #else
@@ -1054,7 +1054,7 @@ CNU(RTS_CONTROL_ENABLE, "Enable RTS flow
 
 */
 
-#if defined (__FreeBSD__) || defined (__linux__) || defined (__DragonFly__)
+#if defined (__FreeBSD__) || defined (__linux__) || defined (__DragonFly__) || defined (__NetBSD__)
 # define PTY_Library "-lutil"
 #else
 # define PTY_Library ""
@@ -1970,6 +1970,7 @@ CND(CLOCK_THREAD_CPUTIME_ID, "Thread CPU
 
 #if defined(__linux__) || defined(__FreeBSD__) \
  || (defined(_AIX) && defined(_AIXVERSION_530)) \
+ || defined(__NetBSD__) \
  || defined(__DragonFly__) || defined(__QNX__)
 /** On these platforms use system provided monotonic clock instead of
  ** the default CLOCK_REALTIME. We then need to set up cond var attributes
@@ -1992,6 +1993,7 @@ CNS(CLOCK_RT_Ada, "")
 #endif
 
 #if defined (__APPLE__) || defined (__linux__) || defined (__ANDROID__) \
+  || defined (__NetBSD__) \
   || defined (__QNX__) || defined (__rtems__) || defined (DUMMY)
 /*
 
