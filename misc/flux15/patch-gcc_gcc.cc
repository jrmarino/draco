--- gcc/gcc.cc.orig	2026-01-25 16:13:22 UTC
+++ gcc/gcc.cc
@@ -1656,6 +1656,9 @@ static const char *const standard_startfile_prefix_2
 static const char *const standard_startfile_prefix_2
   = STANDARD_STARTFILE_PREFIX_2;
 
+/* Since we hardset rpath to LOCALBASE, follow with library search path */
+static const char *const standard_raven_prefix = "@LOCALBASE@/lib/";
+
 /* A relative path to be used in finding the location of tools
    relative to the driver.  */
 static const char *const tooldir_base_prefix = TOOLDIR_BASE_PREFIX;
@@ -5517,6 +5520,8 @@ process_command (unsigned int decoded_options_count,
 #endif
       add_prefix (&startfile_prefixes, standard_exec_prefix, "BINUTILS",
 		  PREFIX_PRIORITY_LAST, 1, 0);
+      add_prefix (&startfile_prefixes, standard_raven_prefix, "BINUTILS",
+		  PREFIX_PRIORITY_LAST, 0, 0);
     }
 
   gcc_assert (!IS_ABSOLUTE_PATH (tooldir_base_prefix));
