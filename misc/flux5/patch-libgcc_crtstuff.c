--- libgcc/crtstuff.c.orig	2015-06-08 14:14:47 UTC
+++ libgcc/crtstuff.c
@@ -81,7 +81,7 @@ call_ ## FUNC (void)					\
 #endif
 
 #if defined(TARGET_DL_ITERATE_PHDR) && \
-   (defined(__DragonFly__) || defined(__FreeBSD__))
+   (defined(__DragonFly__) || defined(__FreeBSD__) || defined(__NetBSD__))
 #define BSD_DL_ITERATE_PHDR_AVAILABLE
 #endif
  
