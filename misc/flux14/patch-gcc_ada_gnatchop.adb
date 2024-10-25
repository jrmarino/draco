--- gcc/ada/gnatchop.adb.orig	2021-08-18 01:05:10 UTC
+++ gcc/ada/gnatchop.adb
@@ -44,7 +44,7 @@ procedure Gnatchop is
    Config_File_Name : constant String_Access := new String'("gnat.adc");
    --  The name of the file holding the GNAT configuration pragmas
 
-   Gcc : String_Access := new String'("gcc");
+   Gcc : String_Access := new String'("ada");
    --  May be modified by switch --GCC=
 
    Gcc_Set : Boolean := False;
