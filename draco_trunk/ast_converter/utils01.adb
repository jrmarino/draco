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


package body utils01 is

   -------------------------
   --  present_llvm_tree  --
   -------------------------

   function present_llvm_tree (TreeAssoc   : in TTreeAssociation;
                               gnat_entity : in Entity_Id)
   return Boolean is
      index  : Natural := Natural (gnat_entity);
      result : Boolean;
   begin
      result := TreeAssoc (index).pointer_type /= loc_unused;
      return result;
   end present_llvm_tree;



   ----------------------
   --  get_llvm_value  --
   ----------------------

   function get_llvm_value    (TreeAssoc   : in TTreeAssociation;
                               gnat_entity : in Entity_Id)
   return LLVMValueRef is
      index  : Natural := Natural (gnat_entity);
   begin
      pragma Assert (
                TreeAssoc (index).pointer_type = loc_value,
                "Tree leaf is not associated with a value."
      );
      return LLVMValueRef (TreeAssoc (index).llvm_pointer);
   end get_llvm_value;



   ---------------------
   --  get_llvm_type  --
   ---------------------

   function get_llvm_type     (TreeAssoc   : in TTreeAssociation;
                               gnat_entity : in Entity_Id)
   return LLVMTypeRef is
      index  : Natural := Natural (gnat_entity);
   begin
      pragma Assert (
                TreeAssoc (index).pointer_type = loc_type,
                "Tree leaf is not associated with a type."
      );
      return LLVMTypeRef (TreeAssoc (index).llvm_pointer);
   end get_llvm_type;



   -----------------------
   --  save_llvm_value  --
   -----------------------

   procedure save_llvm_value  (TreeAssoc   : in out TTreeAssociation;
                               gnat_entity : in Entity_Id;
                               value_ref   : in LLVMValueRef)
   is
      present : Boolean := present_llvm_tree (TreeAssoc, gnat_entity);
      valid   : Boolean := Address (value_ref) /= Null_Address;
      index   : Natural := Natural (gnat_entity);
   begin
      pragma Assert (
                valid and not present,
                "Tree leaf value already present upon save"
      );
      TreeAssoc (index).pointer_type := loc_value;
      TreeAssoc (index).llvm_pointer := Address (value_ref);
   end save_llvm_value;



   ----------------------
   --  save_llvm_type  --
   ----------------------
   
   procedure save_llvm_type   (TreeAssoc   : in out TTreeAssociation;
                               gnat_entity : in Entity_Id;
                               type_ref    : in LLVMTypeRef)
   is
      present : Boolean := present_llvm_tree (TreeAssoc, gnat_entity);
      valid   : Boolean := Address (type_ref) /= Null_Address;
      index   : Natural := Natural (gnat_entity);
   begin
      pragma Assert (
                valid and not present,
                "Tree leaf type already present upon save"
      );
      TreeAssoc (index).pointer_type := loc_type;
      TreeAssoc (index).llvm_pointer := Address (type_ref);
   end save_llvm_type;


end utils01;