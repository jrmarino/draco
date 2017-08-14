--- gcc/dwarf2out.c	2017/08/12 00:28:04	251065
+++ gcc/dwarf2out.c	2017/08/12 09:07:12	251066
@@ -12506,6 +12506,15 @@
 
       if (qualified_type == dtype)
 	{
+	  tree origin = decl_ultimate_origin (name);
+
+	  /* Typedef variants that have an abstract origin don't get their own
+	     type DIE (see gen_typedef_die), so fall back on the ultimate
+	     abstract origin instead.  */
+	  if (origin != NULL)
+	    return modified_type_die (TREE_TYPE (origin), cv_quals, reverse,
+				      context_die);
+
 	  /* For a named type, use the typedef.  */
 	  gen_type_die (qualified_type, context_die);
 	  return lookup_type_die (qualified_type);
@@ -24296,7 +24305,7 @@
 gen_typedef_die (tree decl, dw_die_ref context_die)
 {
   dw_die_ref type_die;
-  tree origin;
+  tree type;
 
   if (TREE_ASM_WRITTEN (decl))
     {
@@ -24305,75 +24314,71 @@
       return;
     }
 
+  /* As we avoid creating DIEs for local typedefs (see decl_ultimate_origin
+     checks in process_scope_var and modified_type_die), this should be called
+     only for original types.  */
+  gcc_assert (decl_ultimate_origin (decl) == NULL);
+
   TREE_ASM_WRITTEN (decl) = 1;
   type_die = new_die (DW_TAG_typedef, context_die, decl);
-  origin = decl_ultimate_origin (decl);
-  if (origin != NULL)
-    add_abstract_origin_attribute (type_die, origin);
-  else
+
+  add_name_and_src_coords_attributes (type_die, decl);
+  if (DECL_ORIGINAL_TYPE (decl))
     {
-      tree type = TREE_TYPE (decl);
+      type = DECL_ORIGINAL_TYPE (decl);
+      if (type == error_mark_node)
+	return;
 
+      gcc_assert (type != TREE_TYPE (decl));
+      equate_type_number_to_die (TREE_TYPE (decl), type_die);
+    }
+  else
+    {
+      type = TREE_TYPE (decl);
       if (type == error_mark_node)
 	return;
 
-      add_name_and_src_coords_attributes (type_die, decl);
-      if (DECL_ORIGINAL_TYPE (decl))
+      if (is_naming_typedef_decl (TYPE_NAME (type)))
 	{
-	  type = DECL_ORIGINAL_TYPE (decl);
-
-	  if (type == error_mark_node)
-	    return;
-
-	  gcc_assert (type != TREE_TYPE (decl));
-	  equate_type_number_to_die (TREE_TYPE (decl), type_die);
+	  /* Here, we are in the case of decl being a typedef naming
+	     an anonymous type, e.g:
+		 typedef struct {...} foo;
+	     In that case TREE_TYPE (decl) is not a typedef variant
+	     type and TYPE_NAME of the anonymous type is set to the
+	     TYPE_DECL of the typedef. This construct is emitted by
+	     the C++ FE.
+
+	     TYPE is the anonymous struct named by the typedef
+	     DECL. As we need the DW_AT_type attribute of the
+	     DW_TAG_typedef to point to the DIE of TYPE, let's
+	     generate that DIE right away. add_type_attribute
+	     called below will then pick (via lookup_type_die) that
+	     anonymous struct DIE.  */
+	  if (!TREE_ASM_WRITTEN (type))
+	    gen_tagged_type_die (type, context_die, DINFO_USAGE_DIR_USE);
+
+	  /* This is a GNU Extension.  We are adding a
+	     DW_AT_linkage_name attribute to the DIE of the
+	     anonymous struct TYPE.  The value of that attribute
+	     is the name of the typedef decl naming the anonymous
+	     struct.  This greatly eases the work of consumers of
+	     this debug info.  */
+	  add_linkage_name_raw (lookup_type_die (type), decl);
 	}
-      else
-	{
-	  if (is_naming_typedef_decl (TYPE_NAME (type)))
-	    {
-	      /* Here, we are in the case of decl being a typedef naming
-	         an anonymous type, e.g:
-	             typedef struct {...} foo;
-	         In that case TREE_TYPE (decl) is not a typedef variant
-	         type and TYPE_NAME of the anonymous type is set to the
-	         TYPE_DECL of the typedef. This construct is emitted by
-	         the C++ FE.
-
-	         TYPE is the anonymous struct named by the typedef
-	         DECL. As we need the DW_AT_type attribute of the
-	         DW_TAG_typedef to point to the DIE of TYPE, let's
-	         generate that DIE right away. add_type_attribute
-	         called below will then pick (via lookup_type_die) that
-	         anonymous struct DIE.  */
-	      if (!TREE_ASM_WRITTEN (type))
-	        gen_tagged_type_die (type, context_die, DINFO_USAGE_DIR_USE);
-
-	      /* This is a GNU Extension.  We are adding a
-		 DW_AT_linkage_name attribute to the DIE of the
-		 anonymous struct TYPE.  The value of that attribute
-		 is the name of the typedef decl naming the anonymous
-		 struct.  This greatly eases the work of consumers of
-		 this debug info.  */
-	      add_linkage_name_raw (lookup_type_die (type), decl);
-	    }
-	}
-
-      add_type_attribute (type_die, type, decl_quals (decl), false,
-			  context_die);
+    }
 
-      if (is_naming_typedef_decl (decl))
-	/* We want that all subsequent calls to lookup_type_die with
-	   TYPE in argument yield the DW_TAG_typedef we have just
-	   created.  */
-	equate_type_number_to_die (type, type_die);
+  add_type_attribute (type_die, type, decl_quals (decl), false,
+		      context_die);
 
-      type = TREE_TYPE (decl);
+  if (is_naming_typedef_decl (decl))
+    /* We want that all subsequent calls to lookup_type_die with
+       TYPE in argument yield the DW_TAG_typedef we have just
+       created.  */
+    equate_type_number_to_die (type, type_die);
 
-      add_alignment_attribute (type_die, type);
+  add_alignment_attribute (type_die, TREE_TYPE (decl));
 
-      add_accessibility_attribute (type_die, decl);
-    }
+  add_accessibility_attribute (type_die, decl);
 
   if (DECL_ABSTRACT_P (decl))
     equate_decl_number_to_die (decl, type_die);
@@ -24485,15 +24490,23 @@
       if (TREE_ASM_WRITTEN (type))
 	return;
 
+      tree name = TYPE_NAME (type);
+      tree origin = decl_ultimate_origin (name);
+      if (origin != NULL)
+	{
+	  gen_decl_die (origin, NULL, NULL, context_die);
+	  return;
+	}
+
       /* Prevent broken recursion; we can't hand off to the same type.  */
-      gcc_assert (DECL_ORIGINAL_TYPE (TYPE_NAME (type)) != type);
+      gcc_assert (DECL_ORIGINAL_TYPE (name) != type);
 
       /* Give typedefs the right scope.  */
       context_die = scope_die_for (type, context_die);
 
       TREE_ASM_WRITTEN (type) = 1;
 
-      gen_decl_die (TYPE_NAME (type), NULL, NULL, context_die);
+      gen_decl_die (name, NULL, NULL, context_die);
       return;
     }
 
@@ -24812,6 +24825,22 @@
   else
     die = NULL;
 
+  /* Avoid creating DIEs for local typedefs and concrete static variables that
+     will only be pruned later.  */
+  if ((origin || decl_ultimate_origin (decl))
+      && (TREE_CODE (decl_or_origin) == TYPE_DECL
+	  || (VAR_P (decl_or_origin) && TREE_STATIC (decl_or_origin))))
+    {
+      origin = decl_ultimate_origin (decl_or_origin);
+      if (decl && VAR_P (decl) && die != NULL)
+	{
+	  die = lookup_decl_die (origin);
+	  if (die != NULL)
+	    equate_decl_number_to_die (decl, die);
+	}
+      return;
+    }
+
   if (die != NULL && die->die_parent == NULL)
     add_child_die (context_die, die);
   else if (TREE_CODE (decl_or_origin) == IMPORTED_DECL)
