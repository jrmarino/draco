--- gcc/config/netbsd-elf.h.orig	2021-10-14 01:03:51 UTC
+++ gcc/config/netbsd-elf.h
@@ -80,6 +80,7 @@ along with GCC; see the file COPYING3.
   "%{assert*} %{R*} %{rpath*} \
    %{shared:-shared} \
    %{symbolic:-Bsymbolic} \
+   %{!static:-rpath @PREFIX@/@GCCAUX@/lib:@LOCALBASE@/lib} \
    %{!shared: \
      -dc -dp \
      %{!nostdlib: \
@@ -101,6 +102,9 @@ along with GCC; see the file COPYING3.
 #undef SUBTARGET_EXTRA_SPECS
 #define SUBTARGET_EXTRA_SPECS   NETBSD_SUBTARGET_EXTRA_SPECS
 
+/* Define this to be nonzero if static stack checking is supported. */
+#define STACK_CHECK_STATIC_BUILTIN 1
+
 /* Use --as-needed -lgcc_s for eh support.  */
 #ifdef HAVE_LD_AS_NEEDED
 #define USE_LD_AS_NEEDED 1
