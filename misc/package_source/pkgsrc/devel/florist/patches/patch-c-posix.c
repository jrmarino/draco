$NetBSD: patch-c-posix.c,v 1.5 2012/07/09 05:52:14 marino Exp $

--- c-posix.c.orig	2012-05-10 13:32:15.000000000 +0000
+++ c-posix.c
@@ -689,6 +689,25 @@ typedef struct siginfo {
 
 /* sigevent must precede aiocb
  */
+#if defined(__DragonFly__)
+/*
+ * union _sigev_info {
+ *  int   sigev_signo;
+ *  int   sigev_notify_kqueue;
+ *  void *sigev_notify_attributes;
+ * };
+ * sizeof(union _sigev_info) = sizeof(int)
+ * Use "int" rather than fooling with union
+ * For simplicity, The Ada part is only going to list sigev_signo
+ * as the other two elements of the union are unused.
+ */
+  GT1(sigevent, 1)
+  GT2(sigev_notify, int)
+  GT2(sigev_signo, int)
+  GT2(sigev_value, union sigval)
+  GT2(sigev_notify_function, void (*)(union sigval))
+  GT3
+#else  /* __DragonFly__ */
 #ifdef HAVE_struct_sigevent
   GT1(sigevent, 1)
 #else
@@ -711,6 +730,7 @@ struct sigevent {
   GT2(sigev_notify_attributes,pthread_attr_t *)
 #endif
   GT3
+#endif /* __DragonFly__ */
 
 #ifdef HAVE_struct_aiocb
   GT1(aiocb, 1)
@@ -5130,9 +5150,9 @@ void create_c() {
  */
 
 #if defined(__APPLE__)
-# define BADSIG 0
+# define FLOR_BADSIG 0
 #else
-# define BADSIG (-1)
+# define FLOR_BADSIG (-1)
 #endif
 {sigset_t set;
   int sig;
@@ -5143,7 +5163,7 @@ void create_c() {
   for (sig = 0; sig < 1024; sig++) {
     result = sigismember (&set, sig);
     if (result == 1) last_good = sig;
-    else if ((result == BADSIG) && (first_bad = -1)) first_bad = sig;
+    else if ((result == FLOR_BADSIG) && (first_bad = -1)) first_bad = sig;
   }
   if (last_good == 1023)
     printf("c-posix: WARNING: signal range estimate probably too small\n");
