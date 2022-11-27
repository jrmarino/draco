--- gcc/ada/libgnat/g-socthi.ads.orig	2021-10-08 14:08:50 UTC
+++ gcc/ada/libgnat/g-socthi.ads
@@ -53,8 +53,6 @@ package GNAT.Sockets.Thin is
 
    package C renames Interfaces.C;
 
-   use type System.CRTL.ssize_t;
-
    function Socket_Errno return Integer renames GNAT.OS_Lib.Errno;
    --  Returns last socket error number
 
@@ -256,7 +254,7 @@ private
    pragma Import (C, C_Getsockname, "getsockname");
    pragma Import (C, C_Getsockopt, "getsockopt");
    pragma Import (C, C_Listen, "listen");
-   pragma Import (C, C_Select, "select");
+   pragma Import (C, C_Select, "__gnat_select");
    pragma Import (C, C_Setsockopt, "setsockopt");
    pragma Import (C, C_Shutdown, "shutdown");
    pragma Import (C, C_Socketpair, "socketpair");
