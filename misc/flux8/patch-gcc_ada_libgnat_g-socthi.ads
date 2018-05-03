--- gcc/ada/libgnat/g-socthi.ads.orig	2018-01-11 08:55:25 UTC
+++ gcc/ada/libgnat/g-socthi.ads
@@ -53,8 +53,6 @@ package GNAT.Sockets.Thin is
 
    package C renames Interfaces.C;
 
-   use type System.CRTL.ssize_t;
-
    function Socket_Errno return Integer renames GNAT.OS_Lib.Errno;
    --  Returns last socket error number
 
