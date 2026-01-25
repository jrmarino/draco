--- gcc/ada/libgnarl/s-taprop__posix.adb.orig
+++ gcc/ada/libgnarl/s-taprop__posix.adb
@@ -610,6 +610,9 @@ package body System.Task_Primitives.Operations is
         or else Priority_Specific_Policy = 'F'
         or else Time_Slice_Val = 0
       then
+         if STATIC_PRIORITY_FOR_SCHEDULER then
+            Param.sched_priority := DEFAULT_SCHEDULER_PRIORITY;
+         end if;
          Result := pthread_setschedparam
            (T.Common.LL.Thread, SCHED_FIFO, Param'Access);
 
