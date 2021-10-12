--- gcc/ada/libgnarl/s-osinte__freebsd.ads.orig	2021-10-12 02:41:57 UTC
+++ gcc/ada/libgnarl/s-osinte__freebsd.ads
@@ -236,6 +236,11 @@ package System.OS_Interface is
    SCHED_OTHER : constant := 2;
    SCHED_RR    : constant := 3;
 
+   --  pthread_setschedparam is implementation-defined for SCHED_OTHER.
+   --  FreeBSD supports the full priority range.
+   STATIC_PRIORITY_FOR_SCHEDULER : constant Boolean := False;
+   DEFAULT_SCHEDULER_PRIORITY    : constant := 0;
+
    function To_Target_Priority
      (Prio : System.Any_Priority) return Interfaces.C.int;
    --  Maps System.Any_Priority to a POSIX priority
