--
--  DRACO ADA COMPILER
--  Misc Functions
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


with Dglobal;
with System; use System;
with Einfo;  use Einfo;

package body Decl is

   -------------------------
   --  get_unpadded_type  --
   -------------------------

   function get_unpadded_type (gnat_entity : Entity_Id)
   return LLVMTypeRef is
      bogus : LLVMTypeRef := LLVMTypeRef (Null_Address);
   begin
      --  TO-DO: Obviously, this function is bogus
      return bogus;
   end get_unpadded_type;



   -------------------------
   --  gnat_to_llvm_type  --
   -------------------------

   function gnat_to_llvm_type (gnat_entity  : Entity_Id)
   return LLVMTypeKind is
      llvm_ref : Address;
   begin
      if Dglobal.type_annotate_only and then Is_Generic_Type (gnat_entity) then
         return LLVMVoidTypeKind;
      end if;

      llvm_ref := gnat_to_llvm_entity (
                     gnat_entity  => gnat_entity,
                     llvm_expr    => Dglobal.NULL_TREE,
                     definition   => none);

      declare
         LPT : Utils01.TLLVMPointerType;
         use type Utils01.TLLVMPointerType;
      begin
         LPT := Utils01.get_pointer_type (
                        TreeAssoc   => Dglobal.ref_TreeSync.all,
                        gnat_entity => gnat_entity);
         pragma Assert (LPT = Utils01.loc_type);
      end;

      return LLVMGetTypeKind (LLVMTypeRef (llvm_ref));

   end gnat_to_llvm_type;



   ---------------------------
   --  gnat_to_llvm_entity  --
   ---------------------------

   function gnat_to_llvm_entity (gnat_entity  : Entity_Id;
                                 llvm_expr    : LLVMValueRef;
                                 definition   : TDefinition)
   return Address is
      bogus : Address := Null_Address;
   begin
      --  TO-DO: Obviously, this function is bogus
      return bogus;
   end gnat_to_llvm_entity;

end Decl;
