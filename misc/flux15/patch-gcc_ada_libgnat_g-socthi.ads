--- gcc/ada/libgnat/g-socthi.ads.orig	2026-01-25 15:54:37 UTC
+++ gcc/ada/libgnat/g-socthi.ads
@@ -53,8 +53,6 @@ package GNAT.Sockets.Thin is
 
    package C renames Interfaces.C;
 
-   use type System.CRTL.ssize_t;
-
    function Socket_Errno return Integer renames GNAT.OS_Lib.Errno;
    --  Returns last socket error number
 
@@ -256,7 +254,7 @@ private
    pragma Inline (C_Getsockname);
    pragma Inline (C_Getsockopt);
    pragma Import (C, C_Listen, "listen");
-   pragma Import (C, C_Select, "select");
+   pragma Import (C, C_Select, "__gnat_select");
    pragma Inline (C_Setsockopt);
    pragma Import (C, C_Shutdown, "shutdown");
    pragma Import (C, C_Socketpair, "socketpair");
