--
--  DRACO ADA COMPILER
--  DLC LLVM Tree Node Structure
--
--
--  Copyright (c) 2010, John Marino (www.auroraux.org)
--  All rights reserved.
--
--  Permission to use, copy, modify, and/or distribute this software for any
--  purpose with or without fee is hereby granted, provided that the above
--  copyright notice and this permission notice appear in all copies.
--
--  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
--  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
--  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
--  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
--  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
--  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
--  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


package body LlvmTree is

   -----------------------------
   --  get_typeref_from_tree  --
   -----------------------------

   function get_typeref_from_tree (tree : TTree)
   return LLVMTypeRef is
   begin
      pragma Assert (tree.pointer_type = loc_type,
                     "llvmtree: Tree is not associated with a type.");

      return LLVMTypeRef (tree.llvm_pointer);
   end get_typeref_from_tree;


   -----------------------
   --  forge_tree_type  --
   -----------------------

   function forge_tree_type (TypeRef : LLVMTypeRef)
   return TTree is
      result : TTree;
   begin
      result.llvm_pointer := Address (TypeRef);
      result.pointer_type := loc_type;
      return result;
   end forge_tree_type;


   -----------------------
   -- forge_tree_value  --
   -----------------------

   function forge_tree_value (TypeRef : LLVMValueRef)
   return TTree is
      result : TTree;
   begin
      result.llvm_pointer := Address (TypeRef);
      result.pointer_type := loc_value;
      return result;
   end forge_tree_value;


end LlvmTree;
