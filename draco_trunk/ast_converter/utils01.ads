--
--  DRACO ADA COMPILER
--  DLC Utilities #1
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


with Core_h;   use Core_h;
with Types;    use Types;
with System;   use System;
with LlvmTree; use LlvmTree;

package Utils01 is

   type TTreeAssociation is array (Node_Id range <>) of TTree;
   --  The intended use is to declare an instance of TTreeAssociation with
   --  default record values of system.null_pointer and loc_unused, and to
   --  pass that variable to the procedures and functions below.

   type TPSync is access all TTreeAssociation;


   function init_gnat_to_llvm (max_gnat_nodes : Node_Id)
   return TTreeAssociation;
   --  This returns a TTreeAssociation variable, which is an array of
   --  records.  The array range will be 0 to max_gnat_nodes - 1 (assuming
   --  the lower bound for Node_Id continues to be zero)


   function present_llvm_tree (TreeAssoc   : TTreeAssociation;
                               gnat_entity : Entity_Id) return Boolean;
   --  Each element of the TreeAssoc array is initialized to pointer_type of
   --  loc_unused.  This function will return false if the pointer_type is
   --  still set to loc_unused, otherwise it will return true.  It basically
   --  signals whether the gnat entity in question has been added to the LLVM
   --  AST yet.


   function get_llvm_tree     (TreeAssoc   : TTreeAssociation;
                               gnat_entity : Entity_Id) return TTree;
   --  If the LLVM AST value is set, this function will return the TTree
   --  structure.  However, if the pointer_type is loc_unused, then
   --  it will assert, as this is never expected.


   function get_llvm_value    (TreeAssoc   : TTreeAssociation;
                               gnat_entity : Entity_Id) return LLVMValueRef;
   --  If the LLVM AST value is set, this function will return the pointer
   --  to it.  However, if the pointer_type is loc_unused or loc_type, then
   --  it will assert, as this is never expected.


   function get_llvm_type     (TreeAssoc   : TTreeAssociation;
                               gnat_entity : Entity_Id) return LLVMTypeRef;
   --  If the LLVM AST type is set, this function will return the pointer
   --  to it.  However, if the pointer_type is loc_unused or loc_value, then
   --  it will assert, as this is never expected.


   function get_pointer_type  (TreeAssoc   : TTreeAssociation;
                               gnat_entity : Entity_Id)
   return TLLVMPointerType;
   --  If the type of pointer stored in the structure is not known for certain,
   --  this function will determine it.  It could possibly be used to
   --  generate assertions.


   procedure save_llvm_tree   (TreeAssoc   : in out TTreeAssociation;
                               gnat_entity : in Entity_Id;
                               tree        : in TTree);
   --  This procedure will associate a gnat AST node with an llvm AST node
   --  which is stored in the TTree structure.  A copy of the llvm node will
   --  be made after verifying the gnat node was previously unset;

end Utils01;
