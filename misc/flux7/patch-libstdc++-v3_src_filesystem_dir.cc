--- libstdc++-v3/src/filesystem/dir.cc.orig	2016-01-04 14:30:50 UTC
+++ libstdc++-v3/src/filesystem/dir.cc
@@ -147,7 +147,13 @@ fs::_Dir::advance(error_code* ec, direct
 
   int err = std::exchange(errno, 0);
   const auto entp = readdir(dirp);
+#ifdef __ANDROID__
+  int tmperr = errno;
+  errno = err;
+  err = tmperr;
+#else
   std::swap(errno, err);
+#endif
 
   if (entp)
     {
