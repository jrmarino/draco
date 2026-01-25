--- gcc/ada/libgnat/g-socthi.ads.orig	2026-01-25 15:54:37 UTC
+++ gcc/ada/libgnat/g-socthi.ads
@@ -256,7 +256,7 @@ private
    pragma Inline (C_Getsockname);
    pragma Inline (C_Getsockopt);
    pragma Import (C, C_Listen, "listen");
-   pragma Import (C, C_Select, "select");
+   pragma Import (C, C_Select, "__gnat_select");
    pragma Inline (C_Setsockopt);
    pragma Import (C, C_Shutdown, "shutdown");
    pragma Import (C, C_Socketpair, "socketpair");
