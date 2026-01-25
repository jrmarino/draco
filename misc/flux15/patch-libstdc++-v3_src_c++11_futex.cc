--- libstdc++-v3/src/c++11/futex.cc.orig	2016-01-04 14:30:50 UTC
+++ libstdc++-v3/src/c++11/futex.cc
@@ -27,7 +27,11 @@
 #if defined(_GLIBCXX_HAVE_LINUX_FUTEX) && ATOMIC_INT_LOCK_FREE > 1
 #include <chrono>
 #include <climits>
+#ifdef __ANDROID__
+#include <sys/syscall.h>
+#else
 #include <syscall.h>
+#endif
 #include <unistd.h>
 #include <sys/time.h>
 #include <errno.h>
