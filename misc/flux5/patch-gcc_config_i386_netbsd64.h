--- gcc/config/i386/netbsd64.h.orig	2015-06-15 08:05:20 UTC
+++ gcc/config/i386/netbsd64.h
@@ -69,3 +69,10 @@ along with GCC; see the file COPYING3.
 #define HAVE_ENABLE_EXECUTE_STACK
 
 #define IX86_MAYBE_NO_LIBGCC_TFMODE
+
+/* Define this to be nonzero if static stack checking is supported. */
+#define STACK_CHECK_STATIC_BUILTIN 1
+
+#if defined(HAVE_LD_EH_FRAME_HDR)
+#define LINK_EH_SPEC "%{!static:--eh-frame-hdr} "
+#endif
