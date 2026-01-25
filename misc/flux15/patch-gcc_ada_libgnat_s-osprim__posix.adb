--- gcc/ada/libgnat/s-osprim__posix.adb.orig	2023-07-05 04:44:39 UTC
+++ gcc/ada/libgnat/s-osprim__posix.adb
@@ -49,7 +49,7 @@ package body System.OS_Primitives is
    pragma Convention (C, timespec);
 
    function nanosleep (rqtp, rmtp : not null access timespec) return Integer;
-   pragma Import (C, nanosleep, "nanosleep");
+   pragma Import (C, nanosleep, "__gnat_nanosleep");
 
    -----------
    -- Clock --
@@ -79,7 +79,7 @@ package body System.OS_Primitives is
       function gettimeofday
         (Tv : access timeval;
          Tz : System.Address := System.Null_Address) return Integer;
-      pragma Import (C, gettimeofday, "gettimeofday");
+      pragma Import (C, gettimeofday, "__gnat_gettimeofday");
 
    begin
       --  The return codes for gettimeofday are as follows (from man pages):
