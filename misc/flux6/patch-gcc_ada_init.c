--- gcc/ada/init.c.orig	2015-11-25 15:16:44 UTC
+++ gcc/ada/init.c
@@ -2510,8 +2510,15 @@ __gnat_adjust_context_for_raise (int sig
 {
   mcontext_t *mcontext = &((ucontext_t *) ucontext)->uc_mcontext;
 
+#if defined (__i386__)
+  unsigned long *pc = (unsigned long *)mcontext->gregs[REG_EIP];
+  /* The pattern is "orl $0x0,(%esp)" for a probe in 32-bit mode.  */
+  if (signo == SIGSEGV && pc && *pc == 0x00240c83)
+    mcontext->gregs[REG_ESP] += 4096 + 4 * sizeof (unsigned long);
+#elif defined (__ARMEL__)
   /* ARM Bump has to be an even number because of odd/even architecture.  */
-  ((mcontext_t *) mcontext)->arm_pc += 2;
+  mcontext->arm_pc += 2;
+#endif
 }
 
 static void
