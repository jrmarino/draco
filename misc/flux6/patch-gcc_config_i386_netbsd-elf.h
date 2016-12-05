--- gcc/config/i386/netbsd-elf.h.orig	2016-12-05 16:31:18 UTC
+++ gcc/config/i386/netbsd-elf.h
@@ -38,7 +38,8 @@ along with GCC; see the file COPYING3.
 /* Provide a LINK_SPEC appropriate for a NetBSD/i386 ELF target.  */
 
 #undef LINK_SPEC
-#define LINK_SPEC NETBSD_LINK_SPEC_ELF
+#define LINK_SPEC NETBSD_LINK_SPEC_ELF \
+"%{!static:-rpath @PREFIX@/gcc-aux/lib}"
 
 #define NETBSD_ENTRY_POINT "__start"
 
@@ -121,3 +122,10 @@ along with GCC; see the file COPYING3.
 #define HAVE_ENABLE_EXECUTE_STACK
 
 #define IX86_MAYBE_NO_LIBGCC_TFMODE
+
+/* Define this to be nonzero if static stack checking is supported. */
+#define STACK_CHECK_STATIC_BUILTIN 1
+
+#if defined(HAVE_LD_EH_FRAME_HDR)
+#define LINK_EH_SPEC "%{!static:--eh-frame-hdr} "
+#endif
