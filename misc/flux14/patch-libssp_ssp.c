--- libssp/ssp.c.orig	2024-10-25 18:18:45 UTC
+++ libssp/ssp.c
@@ -131,7 +131,7 @@ fail (const char *msg1, size_t msg1len,
 
       progname_len = strlen (__progname);
       len = msg1len + progname_len + sizeof(msg2)-1 + 1;
-      p = buf = alloca (len);
+      p = buf = __builtin_alloca (len);
 
       memcpy (p, msg1, msg1len);
       p += msg1len;
