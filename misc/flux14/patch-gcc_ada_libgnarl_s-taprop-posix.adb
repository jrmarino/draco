--- gcc/ada/libgnarl/s-taprop__posix.adb.orig	2021-10-12 02:29:37 UTC
+++ gcc/ada/libgnarl/s-taprop__posix.adb
@@ -629,6 +629,9 @@ package body System.Task_Primitives.Oper
         or else Priority_Specific_Policy = 'F'
         or else Time_Slice_Val = 0
       then
+         if STATIC_PRIORITY_FOR_SCHEDULER then
+            Param.sched_priority := DEFAULT_SCHEDULER_PRIORITY;
+         end if;
          Result := pthread_setschedparam
            (T.Common.LL.Thread, SCHED_FIFO, Param'Access);
 
