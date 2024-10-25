--- gcc/ada/make.adb.orig	2021-08-18 01:32:59 UTC
+++ gcc/ada/make.adb
@@ -445,7 +445,7 @@ package body Make is
    -- Compiler, Binder & Linker Data and Subprograms --
    ----------------------------------------------------
 
-   Gcc      : String_Access := Program_Name ("gcc", "gnatmake");
+   Gcc      : String_Access := Program_Name ("ada", "gnatmake");
    Gnatbind : String_Access := Program_Name ("gnatbind", "gnatmake");
    Gnatlink : String_Access := Program_Name ("gnatlink", "gnatmake");
    --  Default compiler, binder, linker programs
