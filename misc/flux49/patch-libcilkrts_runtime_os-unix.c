--- libcilkrts/runtime/os-unix.c.orig	2014-04-28 04:01:54.000000000 -0500
+++ libcilkrts/runtime/os-unix.c
@@ -56,6 +56,12 @@
     // Uses sysconf(_SC_NPROCESSORS_ONLN) in verbose output
 #elif defined  __FreeBSD__
 // No additional include files
+#elif defined __DragonFly__
+// No additional include files
+#elif defined __NetBSD__
+// No additional include files
+#elif defined __OpenBSD__
+// No additional include files
 #elif defined __CYGWIN__
 // Cygwin on Windows - no additional include files
 #elif defined  __VXWORKS__
@@ -374,7 +380,8 @@ COMMON_SYSDEP int __cilkrts_hardware_cpu
     assert((unsigned)count == count);
 
     return count;
-#elif defined  __FreeBSD__ || defined __CYGWIN__
+#elif defined __FreeBSD__ || defined __CYGWIN__ || defined __DragonFly__ \
+   || defined __NetBSD__ || defined __OpenBSD__
     int ncores = sysconf(_SC_NPROCESSORS_ONLN);
 
     return ncores;
@@ -402,6 +409,12 @@ COMMON_SYSDEP void __cilkrts_yield(void)
     // On MacOS, call sched_yield to yield quantum.  I'm not sure why we
     // don't do this on Linux also.
     sched_yield();
+#elif defined(__DragonFly__)
+    sched_yield();
+#elif defined(__NetBSD__)
+    sched_yield();
+#elif defined(__OpenBSD__)
+    sched_yield();
 #elif defined(__MIC__)
     // On MIC, pthread_yield() really trashes things.  Arch's measurements
     // showed that calling _mm_delay_32() (or doing nothing) was a better
