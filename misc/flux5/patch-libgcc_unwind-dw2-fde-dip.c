--- libgcc/unwind-dw2-fde-dip.c.orig	2015-06-08 14:25:27 UTC
+++ libgcc/unwind-dw2-fde-dip.c
@@ -65,6 +65,12 @@
 #endif
 
 #if !defined(inhibit_libc) && defined(HAVE_LD_EH_FRAME_HDR) \
+    && defined(TARGET_DL_ITERATE_PHDR) && defined(__NetBSD__))
+# define ElfW(type) Elf_##type
+# define USE_PT_GNU_EH_FRAME
+#endif
+
+#if !defined(inhibit_libc) && defined(HAVE_LD_EH_FRAME_HDR) \
     && defined(__OpenBSD__)
 # define ElfW(type) Elf_##type
 # define USE_PT_GNU_EH_FRAME
