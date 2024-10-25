--- gcc/ada/s-oscons-tmplt.c.orig	2024-10-25 15:58:21 UTC
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
@@ -1975,6 +1975,7 @@ CND(CLOCK_THREAD_CPUTIME_ID, "Thread CPU
 
 #if defined(__linux__) || defined(__FreeBSD__) \
  || (defined(_AIX) && defined(_AIXVERSION_530)) \
+ || defined(__NetBSD__) \
  || defined(__DragonFly__) || defined(__QNX__) \
  || defined (__vxworks)
 /** On these platforms use system provided monotonic clock instead of
@@ -1998,6 +1999,7 @@ CNS(CLOCK_RT_Ada, "")
 #endif
 
 #if defined (__APPLE__) || defined (__linux__) || defined (__ANDROID__) \
+  || defined (__NetBSD__) \
   || defined (__QNX__) || defined (__rtems__) || defined (DUMMY)
 /*
 
