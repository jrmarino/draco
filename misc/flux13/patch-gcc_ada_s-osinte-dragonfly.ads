--- gcc/ada/libgnarl/s-osinte__dragonfly.ads.orig	2023-07-05 04:23:08 UTC
+++ gcc/ada/libgnarl/s-osinte__dragonfly.ads
@@ -238,6 +238,11 @@ package System.OS_Interface is
    SCHED_OTHER : constant := 2;
    SCHED_RR    : constant := 3;
 
+   --  pthread_setschedparam is implementation-defined for SCHED_OTHER.
+   --  DragonFly supports the full priority range.
+   STATIC_PRIORITY_FOR_SCHEDULER : constant Boolean := False;
+   DEFAULT_SCHEDULER_PRIORITY    : constant := 0;
+
    function To_Target_Priority
      (Prio : System.Any_Priority) return Interfaces.C.int;
    --  Maps System.Any_Priority to a POSIX priority
