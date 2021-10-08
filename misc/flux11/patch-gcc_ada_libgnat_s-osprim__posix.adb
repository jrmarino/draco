--- gcc/ada/libgnat/s-osprim__posix.adb.orig	2021-10-08 00:12:42.291851000 -0500
+++ gcc/ada/libgnat/s-osprim__posix.adb	2021-10-08 00:14:15.638426000 -0500
@@ -47,7 +47,7 @@
    pragma Convention (C, timespec);
 
    function nanosleep (rqtp, rmtp : not null access timespec) return Integer;
-   pragma Import (C, nanosleep, "nanosleep");
+   pragma Import (C, nanosleep, "__gnat_nanosleep");
 
    -----------
    -- Clock --
@@ -77,7 +77,7 @@
       function gettimeofday
         (Tv : access timeval;
          Tz : System.Address := System.Null_Address) return Integer;
-      pragma Import (C, gettimeofday, "gettimeofday");
+      pragma Import (C, gettimeofday, "__gnat_gettimeofday");
 
    begin
       --  The return codes for gettimeofday are as follows (from man pages):
