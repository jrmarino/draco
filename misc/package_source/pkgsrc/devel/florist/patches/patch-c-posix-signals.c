$NetBSD: patch-c-posix-signals.c,v 1.1 2012/07/09 05:52:14 marino Exp $

--- c-posix-signals.c.orig	2012-05-10 13:32:15.000000000 +0000
+++ c-posix-signals.c
@@ -467,9 +467,9 @@ int guess_nsigs () {
  */
 
 #if defined(__APPLE__)
-# define BADSIG (0)
+# define FLOR_BADSIG (0)
 #else
-# define BADSIG (-1)
+# define FLOR_BADSIG (-1)
 #endif
 
    sigset_t set;
@@ -483,7 +483,7 @@ int guess_nsigs () {
       result = sigismember (&set, sig);
       if (result == 1) {
          last_good = sig;
-      } else if ((result == BADSIG) && (first_bad == -1)) {
+      } else if ((result == FLOR_BADSIG) && (first_bad == -1)) {
          if (sig == 0) {
             fprintf (stderr, "WARNING: C library problem? "
              "sigfillset does not include zero\n");
