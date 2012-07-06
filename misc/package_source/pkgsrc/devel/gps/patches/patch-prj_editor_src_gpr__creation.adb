$NetBSD$

--- prj_editor/src/gpr_creation.adb.orig	2010-03-30 08:12:23.000000000 +0000
+++ prj_editor/src/gpr_creation.adb
@@ -571,6 +571,7 @@ package body GPR_Creation is
       Current_Project : Integer;
       All_Source_Dirs : Boolean := False)
    is
+      pragma Unreferenced (Root_Project);
       Current_Dir : Natural;
       Tmp         : Import_Project_Error;
    begin
