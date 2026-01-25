--- gcc/ada/libgnarl/s-osinte__android.ads.orig
+++ gcc/ada/libgnarl/s-osinte__android.ads
@@ -241,6 +241,11 @@ package System.OS_Interface is
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
