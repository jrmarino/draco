--- gcc/gcc.cc.orig	2023-09-05 02:06:37 UTC
+++ gcc/gcc.cc
@@ -1630,6 +1630,9 @@ static const char *const standard_startf
 static const char *const standard_startfile_prefix_2
   = STANDARD_STARTFILE_PREFIX_2;
 
+/* Since we hardset rpath to LOCALBASE, follow with library search path */
+static const char *const standard_raven_prefix = "@LOCALBASE@/lib/";
+
 /* A relative path to be used in finding the location of tools
    relative to the driver.  */
 static const char *const tooldir_base_prefix = TOOLDIR_BASE_PREFIX;
@@ -5363,6 +5366,8 @@ process_command (unsigned int decoded_op
 #endif
       add_prefix (&startfile_prefixes, standard_exec_prefix, "BINUTILS",
 		  PREFIX_PRIORITY_LAST, 1, 0);
+      add_prefix (&startfile_prefixes, standard_raven_prefix, "BINUTILS",
+		  PREFIX_PRIORITY_LAST, 0, 0);
     }
 
   gcc_assert (!IS_ABSOLUTE_PATH (tooldir_base_prefix));
@@ -7904,16 +7909,6 @@ is_directory (const char *path1, bool li
   *cp++ = '.';
   *cp = '\0';
 
-  /* Exclude directories that the linker is known to search.  */
-  if (linker
-      && IS_DIR_SEPARATOR (path[0])
-      && ((cp - path == 6
-	   && filename_ncmp (path + 1, "lib", 3) == 0)
-	  || (cp - path == 10
-	      && filename_ncmp (path + 1, "usr", 3) == 0
-	      && IS_DIR_SEPARATOR (path[4])
-	      && filename_ncmp (path + 5, "lib", 3) == 0)))
-    return 0;
 
   return (stat (path, &st) >= 0 && S_ISDIR (st.st_mode));
 }
