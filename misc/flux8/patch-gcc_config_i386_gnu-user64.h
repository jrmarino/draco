--- gcc/config/i386/gnu-user64.h.orig	2018-05-03 15:12:22 UTC
+++ gcc/config/i386/gnu-user64.h
@@ -57,6 +57,7 @@ see the files COPYING3 and COPYING.RUNTI
                    %{" SPEC_32 ":-m " GNU_USER_LINK_EMULATION32 "} \
                    %{" SPEC_X32 ":-m " GNU_USER_LINK_EMULATIONX32 "} \
   %{shared:-shared} \
+  %{!static:--enable-new-dtags -rpath @PREFIX@/@GCCAUX@/lib64:@LOCALBASE@/lib} \
   %{!shared: \
     %{!static: \
       %{!static-pie: \
