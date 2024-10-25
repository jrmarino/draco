--- gcc/ada/libgnarl/s-osinte__android.ads.orig	2023-07-05 04:23:01 UTC
+++ gcc/ada/libgnarl/s-osinte__android.ads
@@ -237,6 +237,11 @@ package System.OS_Interface is
    SCHED_FIFO  : constant := 1;
    SCHED_RR    : constant := 2;
 
+   --  pthread_setschedparam is implementation-defined for SCHED_OTHER.
+   --  Android supports the full priority range.
+   STATIC_PRIORITY_FOR_SCHEDULER : constant Boolean := False;
+   DEFAULT_SCHEDULER_PRIORITY    : constant := 0;
+
    function To_Target_Priority
      (Prio : System.Any_Priority)
       return Interfaces.C.int is (Interfaces.C.int (Prio));
