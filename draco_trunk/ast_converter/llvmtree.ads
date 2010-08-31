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


with Core_h; use Core_h;
with System; use System;

package LlvmTree is

   type TLLVMPointerType is (loc_unused,
                             loc_null_tree,
                             loc_value,
                             loc_type,
                             loc_module,
                             loc_basic_block);
   --  Not used: loc_context, loc_builder, loc_type_handle

   type TTypeLangFlags is array (Natural range 0 .. 6) of Boolean;

   type TTree is record
      llvm_pointer   : Address           := Null_Address;
      pointer_type   : TLLVMPointerType  := loc_unused;
      type_lang_flag : TTypeLangFlags    := (others => False);
   end record;

   NULL_TREE : constant TTree := (
                  llvm_pointer   => Null_address,
                  pointer_type   => loc_null_tree,
                  type_lang_flag => (others => False));

   function get_typeref_from_tree (tree : TTree) return LLVMTypeRef;
   --  Given a TTree structure, returns a LLVMTypeRef, which is a pointer to
   --  a type definition with the LLVM AST tree.  If the tree pointer_type is
   --  not equal to loc_type, an assertion will occur.

end LlvmTree;
