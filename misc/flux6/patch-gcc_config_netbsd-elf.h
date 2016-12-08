--- gcc/config/netbsd-elf.h.orig	2016-12-08 05:22:42 UTC
+++ gcc/config/netbsd-elf.h
@@ -71,6 +71,7 @@ along with GCC; see the file COPYING3.
   "%{assert*} %{R*} %{rpath*} \
    %{shared:-shared} \
    %{symbolic:-Bsymbolic} \
+   %{!static:-rpath @PREFIX@/@GCCAUX@/lib} \
    %{!shared: \
      -dc -dp \
      %{!nostdlib: \
@@ -85,3 +86,10 @@ along with GCC; see the file COPYING3.
 #ifdef HAVE_LD_AS_NEEDED
 #define USE_LD_AS_NEEDED 1
 #endif
+
+/* Define this to be nonzero if static stack checking is supported. */
+#define STACK_CHECK_STATIC_BUILTIN 1
+
+#if defined(HAVE_LD_EH_FRAME_HDR)
+#define LINK_EH_SPEC "%{!static:--eh-frame-hdr} "
+#endif
