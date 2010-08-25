--
--  DRACO ADA COMPILER
--  Draco / LLVM AST Converter (DLC)
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


package body DLC is

   ----------------------------------
   --  Draco_to_llvm_ast_converter --
   ----------------------------------

   procedure Draco_to_llvm_ast_converter (
         gnat_root               : in Node_Id;
         max_gnat_node           : in Node_Id;
         next_node_ptr           : in Address;
         prev_node_ptr           : in Address;
         elists_ptr              : in Address;
         elmts_ptr               : in Address;
         strings_ptr             : in Address;
         string_chars_ptr        : in Address;
         list_headers_ptr        : in Address;
         file_info_ptr           : in Address;
         number_file             : in Nat;
         dlc_std_boolean         : in Entity_Id;
         dlc_std_integer         : in Entity_Id;
         dlc_std_character       : in Entity_Id;
         dlc_std_long_long_float : in Entity_Id;
         dlc_std_exception_type  : in Entity_Id;
         dlc_operating_mode      : in DLC_Mode_Type
   ) is
   begin
      null;
   end Draco_to_llvm_ast_converter;



   -------------------------
   --  Identifier_to_llvm --
   -------------------------

   function Identifier_to_llvm (gnat_node : Node_Id)
   return LLVMValueRef is
      result : LLVMValueRef := LLVMValueRef (Null_Address);
   begin
      return result;
   end Identifier_to_llvm;



   ----------------------
   --  Pragma_to_llvm  --
   ----------------------

   function Pragma_to_llvm  (gnat_node : Node_Id)
   return LLVMValueRef is
      result : LLVMValueRef := LLVMValueRef (Null_Address);
   begin
      return result;
   end Pragma_to_llvm;



   -------------------------
   --  Attribute_to_llvm  --
   -------------------------

   function Attribute_to_llvm (gnat_node : Node_Id;
                               attribute  : Attribute_Id)
   return LLVMValueRef is
      result : LLVMValueRef := LLVMValueRef (Null_Address);
   begin
      return result;
   end Attribute_to_llvm;



   ------------------------------
   --  Case_Statement_to_llvm  --
   ------------------------------

   function Case_Statement_to_llvm (gnat_node : Node_Id)
   return LLVMValueRef is
      result : LLVMValueRef := LLVMValueRef (Null_Address);
   begin
      return result;
   end Case_Statement_to_llvm;



   ------------------------------
   --  Loop_Statement_to_llvm  --
   ------------------------------

   function Loop_Statement_to_llvm (gnat_node : Node_Id)
   return LLVMValueRef is
      result : LLVMValueRef := LLVMValueRef (Null_Address);
   begin
      return result;
   end Loop_Statement_to_llvm;



   -------------------------------
   --  Subprogram_body_to_llvm  --
   -------------------------------

   procedure Subprogram_body_to_llvm (gnat_node : in Node_Id)
   is
   begin
      null;
   end Subprogram_body_to_llvm;


   --------------------
   --  call_to_llvm  --
   --------------------

   function call_to_llvm (gnat_node  : Node_Id;
                          llvm_target : LLVMValueRef)
   return LLVMValueRef is
      result : LLVMValueRef := LLVMValueRef (Null_Address);
   begin
      return result;
   end call_to_llvm;



   ----------------------------------------------
   --  Handled_Sequence_Of_Statements_to_llvm  --
   ----------------------------------------------

   function Handled_Sequence_Of_Statements_to_llvm (gnat_node : Node_Id)
   return LLVMValueRef is
      result : LLVMValueRef := LLVMValueRef (Null_Address);
   begin
      return result;
   end Handled_Sequence_Of_Statements_to_llvm;



   ---------------------------------
   --  Exception_Handler_to_llvm  --
   ---------------------------------

   function Exception_Handler_to_llvm (gnat_node : Node_Id)
   return LLVMValueRef is
      result : LLVMValueRef := LLVMValueRef (Null_Address);
   begin
      return result;
   end Exception_Handler_to_llvm;



   --------------------------------
   --  Compilation_Unit_to_llvm  --
   --------------------------------

   procedure Compilation_Unit_to_llvm (gnat_node : Node_Id)
   is
   begin
      null;
   end Compilation_Unit_to_llvm;



   --------------------
   --  gnat_to_llvm  --
   --------------------

   function gnat_to_llvm (gnat_node : Node_Id)
   return LLVMValueRef is
      result : LLVMValueRef := LLVMValueRef (Null_Address);
   begin
      return result;
   end gnat_to_llvm;



end DLC;
