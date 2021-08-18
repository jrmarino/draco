--- gcc/gcc.c.orig
+++ gcc/gcc.c
@@ -1631,6 +1631,9 @@
 static const char *const standard_startfile_prefix_2
   = STANDARD_STARTFILE_PREFIX_2;
 
+/* Since we hardset rpath to LOCALBASE, follow with library search path */
+static const char *const standard_raven_prefix = "@LOCALBASE@/lib/";
+
 /* A relative path to be used in finding the location of tools
    relative to the driver.  */
 static const char *const tooldir_base_prefix = TOOLDIR_BASE_PREFIX;
@@ -5282,6 +5285,8 @@
 #endif
       add_prefix (&startfile_prefixes, standard_exec_prefix, "BINUTILS",
 		  PREFIX_PRIORITY_LAST, 1, 0);
+      add_prefix (&startfile_prefixes, standard_raven_prefix, "BINUTILS",
+		  PREFIX_PRIORITY_LAST, 0, 0);
     }
 
   gcc_assert (!IS_ABSOLUTE_PATH (tooldir_base_prefix));
