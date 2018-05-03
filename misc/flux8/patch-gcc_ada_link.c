--- gcc/ada/link.c.orig	2018-05-03 17:24:27 UTC
+++ gcc/ada/link.c
@@ -104,7 +104,7 @@ unsigned char __gnat_separate_run_path_o
 const char *__gnat_default_libgcc_subdir = "lib";
 
 #elif defined (__FreeBSD__) || defined (__DragonFly__) \
-   || defined (__NetBSD__) || defined (__OpenBSD__)
+   || defined (__NetBSD__) || defined (__OpenBSD__) \
    || defined (__QNX__)
 const char *__gnat_object_file_option = "-Wl,@";
 const char *__gnat_run_path_option = "-Wl,-rpath,";
