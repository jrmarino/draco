--- gcc/expmed.c.orig	2016-12-07 22:54:14.000000000 +0000
+++ gcc/expmed.c	2017-02-05 16:36:12.485427000 +0000
@@ -2286,11 +2286,13 @@
    and AMOUNT the rtx for the amount to shift by.
    Store the result in the rtx TARGET, if that is convenient.
    If UNSIGNEDP is nonzero, do a logical shift; otherwise, arithmetic.
-   Return the rtx for where the value is.  */
+   Return the rtx for where the value is.
+   If that cannot be done, abort the compilation unless MAY_FAIL is true,
+   in which case 0 is returned.  */
 
 static rtx
 expand_shift_1 (enum tree_code code, machine_mode mode, rtx shifted,
-		rtx amount, rtx target, int unsignedp)
+		rtx amount, rtx target, int unsignedp, bool may_fail = false)
 {
   rtx op1, temp = 0;
   int left = (code == LSHIFT_EXPR || code == LROTATE_EXPR);
@@ -2487,7 +2489,7 @@
 	 define_expand for lshrsi3 was added to vax.md.  */
     }
 
-  gcc_assert (temp);
+  gcc_assert (temp != NULL_RTX || may_fail);
   return temp;
 }
 
@@ -2506,6 +2508,16 @@
 			 shifted, GEN_INT (amount), target, unsignedp);
 }
 
+/* Likewise, but return 0 if that cannot be done.  */
+
+static rtx
+maybe_expand_shift (enum tree_code code, machine_mode mode, rtx shifted,
+		    int amount, rtx target, int unsignedp)
+{
+  return expand_shift_1 (code, mode,
+			 shifted, GEN_INT (amount), target, unsignedp, true);
+}
+
 /* Output a shift instruction for expression code CODE,
    with SHIFTED being the rtx for the value to shift,
    and AMOUNT the tree for the amount to shift by.
@@ -5811,11 +5823,12 @@
       if (rtx_equal_p (subtarget, op0))
 	subtarget = 0;
 
-      tem = expand_shift (RSHIFT_EXPR, mode, op0,
-			  GET_MODE_BITSIZE (mode) - 1,
-			  subtarget, 0);
-      tem = expand_binop (mode, sub_optab, tem, op0, subtarget, 0,
-			  OPTAB_WIDEN);
+      tem = maybe_expand_shift (RSHIFT_EXPR, mode, op0,
+				GET_MODE_BITSIZE (mode) - 1,
+				subtarget, 0);
+      if (tem)
+	tem = expand_binop (mode, sub_optab, tem, op0, subtarget, 0,
+			    OPTAB_WIDEN);
     }
 
   if (code == EQ || code == NE)
@@ -5877,9 +5890,9 @@
     }
 
   if (tem && normalizep)
-    tem = expand_shift (RSHIFT_EXPR, mode, tem,
-			GET_MODE_BITSIZE (mode) - 1,
-			subtarget, normalizep == 1);
+    tem = maybe_expand_shift (RSHIFT_EXPR, mode, tem,
+			      GET_MODE_BITSIZE (mode) - 1,
+			      subtarget, normalizep == 1);
 
   if (tem)
     {
