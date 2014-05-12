--- gcc/builtins.c.orig	2014-05-12 14:37:08.000000000 -0500
+++ gcc/builtins.c
@@ -125,9 +125,11 @@ static rtx expand_builtin_memcpy (tree,
 static rtx expand_builtin_mempcpy (tree, rtx, enum machine_mode);
 static rtx expand_builtin_mempcpy_args (tree, tree, tree, rtx,
 					enum machine_mode, int);
+#ifndef NO_UNSAFE_BUILTINS
 static rtx expand_builtin_strcpy (tree, rtx);
 static rtx expand_builtin_strcpy_args (tree, tree, rtx);
 static rtx expand_builtin_stpcpy (tree, rtx, enum machine_mode);
+#endif
 static rtx expand_builtin_strncpy (tree, rtx);
 static rtx builtin_memset_gen_str (void *, HOST_WIDE_INT, enum machine_mode);
 static rtx expand_builtin_memset (tree, rtx, enum machine_mode);
@@ -3370,6 +3372,7 @@ expand_builtin_mempcpy_args (tree dest,
 # define CODE_FOR_movstr CODE_FOR_nothing
 #endif
 
+#ifndef NO_UNSAFE_BUILTINS
 /* Expand into a movstr instruction, if one is available.  Return NULL_RTX if
    we failed, the caller should emit a normal call, otherwise try to
    get the result in TARGET, if convenient.  If ENDP is 0 return the
@@ -3521,6 +3524,7 @@ expand_builtin_stpcpy (tree exp, rtx tar
       return expand_movstr (dst, src, target, /*endp=*/2);
     }
 }
+#endif
 
 /* Callback routine for store_by_pieces.  Read GET_MODE_BITSIZE (MODE)
    bytes from constant string DATA + OFFSET and return it as target
@@ -6140,9 +6144,11 @@ expand_builtin (tree exp, rtx target, rt
       break;
 
     case BUILT_IN_STRCPY:
+#ifndef NO_UNSAFE_BUILTINS
       target = expand_builtin_strcpy (exp, target);
       if (target)
 	return target;
+#endif
       break;
 
     case BUILT_IN_STRNCPY:
@@ -6152,9 +6158,11 @@ expand_builtin (tree exp, rtx target, rt
       break;
 
     case BUILT_IN_STPCPY:
+#ifndef NO_UNSAFE_BUILTINS
       target = expand_builtin_stpcpy (exp, target, mode);
       if (target)
 	return target;
+#endif
       break;
 
     case BUILT_IN_MEMCPY:
