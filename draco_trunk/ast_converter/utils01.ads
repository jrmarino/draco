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


with Core_h; use Core_h;
with Types;  use Types;
with System; use System;

package utils01 is

   type TLLVMPointerType is (loc_unused, loc_value, loc_type);

   type TAssocRec is record
      llvm_pointer : Address;
      pointer_type : TLLVMPointerType;
   end record;

   type TTreeAssociation is array (Natural range <>) of TAssocRec;
   --  The intended use is to declare an instance of TTreeAssociation with
   --  default record values of system.null_pointer and loc_unused, and to
   --  pass that variable to the procedures and functions below.


   function present_llvm_tree (TreeAssoc   : in TTreeAssociation;
                               gnat_entity : in Entity_Id) return Boolean;
   --  Each element of the TreeAssoc array is initialized to pointer_type of
   --  loc_unused.  This function will return false if the pointer_type is
   --  still set to loc_unused, otherwise it will return true.  It basically
   --  signals whether the gnat entity in question has been added to the LLVM
   --  AST yet.


   function get_llvm_value    (TreeAssoc   : in TTreeAssociation;
                               gnat_entity : in Entity_Id) return LLVMValueRef;
   --  If the LLVM AST value is set, this function will return the pointer
   --  to it.  However, if the pointer_type is loc_unused or loc_type, then
   --  it will assert, as this is never expected.


   function get_llvm_type     (TreeAssoc   : in TTreeAssociation;
                               gnat_entity : in Entity_Id) return LLVMTypeRef;
   --  If the LLVM AST type is set, this function will return the pointer
   --  to it.  However, if the pointer_type is loc_unused or loc_value, then
   --  it will assert, as this is never expected.


   procedure save_llvm_value  (TreeAssoc   : in out TTreeAssociation;
                               gnat_entity : in Entity_Id;
                               value_ref   : in LLVMValueRef);
   --  This procedure will associate a gnat value with an llvm value through
   --  the use of a pointer.  A check will be performed to make sure the
   --  association wasn't previously made, and the pointer isn't null.


   procedure save_llvm_type   (TreeAssoc   : in out TTreeAssociation;
                               gnat_entity : in Entity_Id;
                               type_ref    : in LLVMTypeRef);
   --  This procedure will associate a gnat type with an llvm type through
   --  the use of a pointer.  A check will be performed to make sure the
   --  association wasn't previously made, and the pointer isn't null.


end utils01;