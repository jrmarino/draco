--- gcc/gcc.cc.orig	2022-11-27 19:37:49 UTC
+++ gcc/gcc.cc
@@ -1638,6 +1638,9 @@ static const char *const standard_startf
 static const char *const standard_startfile_prefix_2
   = STANDARD_STARTFILE_PREFIX_2;
 
+/* Since we hardset rpath to LOCALBASE, follow with library search path */
+static const char *const standard_raven_prefix = "@LOCALBASE@/lib/";
+
 /* A relative path to be used in finding the location of tools
    relative to the driver.  */
 static const char *const tooldir_base_prefix = TOOLDIR_BASE_PREFIX;
@@ -5399,6 +5402,8 @@ process_command (unsigned int decoded_op
 #endif
       add_prefix (&startfile_prefixes, standard_exec_prefix, "BINUTILS",
 		  PREFIX_PRIORITY_LAST, 1, 0);
+      add_prefix (&startfile_prefixes, standard_raven_prefix, "BINUTILS",
+		  PREFIX_PRIORITY_LAST, 0, 0);
     }
 
   gcc_assert (!IS_ABSOLUTE_PATH (tooldir_base_prefix));
