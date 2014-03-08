--- libstdc++-v3/acinclude.m4.orig	2014-01-26 06:23:07.000000000 -0500
+++ libstdc++-v3/acinclude.m4
@@ -1989,6 +1989,9 @@ AC_DEFUN([GLIBCXX_ENABLE_CLOCALE], [
       darwin* | freebsd*)
 	enable_clocale_flag=darwin
 	;;
+      dragonfly*)
+	enable_clocale_flag=dragonfly
+	;;
       openbsd*)
 	enable_clocale_flag=newlib
 	;;
@@ -2075,6 +2078,23 @@ AC_DEFUN([GLIBCXX_ENABLE_CLOCALE], [
       CMESSAGES_H=config/locale/generic/messages_members.h
       CMESSAGES_CC=config/locale/generic/messages_members.cc
       CMONEY_CC=config/locale/generic/monetary_members.cc
+      CNUMERIC_CC=config/locale/generic/numeric_members.cc
+      CTIME_H=config/locale/generic/time_members.h
+      CTIME_CC=config/locale/generic/time_members.cc
+      CLOCALE_INTERNAL_H=config/locale/generic/c++locale_internal.h
+      ;;
+
+    dragonfly)
+      AC_MSG_RESULT(dragonfly)
+
+      CLOCALE_H=config/locale/generic/c_locale.h
+      CLOCALE_CC=config/locale/dragonfly/c_locale.cc
+      CCODECVT_CC=config/locale/generic/codecvt_members.cc
+      CCOLLATE_CC=config/locale/generic/collate_members.cc
+      CCTYPE_CC=config/locale/dragonfly/ctype_members.cc
+      CMESSAGES_H=config/locale/generic/messages_members.h
+      CMESSAGES_CC=config/locale/generic/messages_members.cc
+      CMONEY_CC=config/locale/generic/monetary_members.cc
       CNUMERIC_CC=config/locale/generic/numeric_members.cc
       CTIME_H=config/locale/generic/time_members.h
       CTIME_CC=config/locale/generic/time_members.cc
