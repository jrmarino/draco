--- gcc/ada/gnatlink.adb.orig	2021-08-18 01:29:54 UTC
+++ gcc/ada/gnatlink.adb
@@ -136,7 +136,7 @@ procedure Gnatlink is
    --  This table collects the arguments to be passed to compile the binder
    --  generated file.
 
-   Gcc : String_Access := Program_Name ("gcc", "gnatlink");
+   Gcc : String_Access := Program_Name ("ada", "gnatlink");
 
    Read_Mode : constant String := "r" & ASCII.NUL;
 
