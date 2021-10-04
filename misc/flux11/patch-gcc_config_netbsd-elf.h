--- gcc/config/netbsd-elf.h.orig	2021-07-28 06:55:07.112284902 +0000
+++ gcc/config/netbsd-elf.h	2021-10-03 13:44:36.569483651 +0000
@@ -80,6 +80,7 @@
   "%{assert*} %{R*} %{rpath*} \
    %{shared:-shared} \
    %{symbolic:-Bsymbolic} \
+   %{!static:-rpath @PREFIX@/@GCCAUX@/lib:@LOCALBASE@/lib} \
    %{!shared: \
      -dc -dp \
      %{!nostdlib: \
