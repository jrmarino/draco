$NetBSD$

--- refactoring/core/src/refactoring-services.adb.orig	2010-10-01 14:01:51.000000000 +0000
+++ refactoring/core/src/refactoring-services.adb
@@ -1318,6 +1318,7 @@ package body Refactoring.Services is
       Direction : Integer := 1) return Editor_Location'Class
    is
       Loc : Editor_Location'Class := From;
+      pragma Unreferenced (Direction);
       Seen_Comment : Boolean := False;
    begin
       loop
