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


with Core_h; use Core_h;
with Types;  use Types;
with Snames; use Snames;
with System; use System;


package DLC is

   type DLC_Mode_Type is (
      Generate_Object,
      --  Full back end operation with object file generation

      Declarations_Only,
      --  Partial back end operation with no object file generation. In this
      --  mode the only useful action performed by DLC is to process all
      --  declarations issuing any error messages (in particular those to
      --  do with rep clauses), and to back annotate representation info.

      Skip
      --  Back end call is skipped (syntax only, or errors found)
   );



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
   );
   --  This is the main program of the back-end.  It sets up all the table
   --  structures and then generates code.



private

   function Identifier_to_llvm (gnat_node : Node_Id) return LLVMValueRef;
   --  Subroutine of DLC to translate gnat_node, an N_Identifier,
   --  to a LLVM tree, which is returned.



   function Pragma_to_llvm  (gnat_node : Node_Id) return LLVMValueRef;
   --  Subroutine of DLC to process gnat_node, an N_Pragma.  Return
   --  any statements generated.



   function Attribute_to_llvm (gnat_node : Node_Id;
                               attribute  : Attribute_Id) return LLVMValueRef;
   --  Subroutine of DLC to translate gnat_node, an N_Attribute node,
   --  to a LLVM tree, which is returned.  Attribute is the attribute ID.



   function Case_Statement_to_llvm (gnat_node : Node_Id) return LLVMValueRef;
   --  Subroutine of DLC to translate gnat_node, an N_Case_Statement,
   --  to a LLVM tree, which is returned.



   function Loop_Statement_to_llvm (gnat_node : Node_Id) return LLVMValueRef;
   --  Subroutine of DLC to translate gnat_node, an N_Loop_Statement,
   --  to a LLVM tree, which is returned.



   procedure Subprogram_body_to_llvm (gnat_node : in Node_Id);
   --  Subroutine of DLC to process gnat_node, an N_Subprogram_Body.  Nothing
   --  is returned.



   function call_to_llvm (gnat_node  : Node_Id;
                          llvm_target : LLVMValueRef) return LLVMValueRef;
   --  Subroutine of DLC to translate gnat_node, either an N_Function_Call
   --  or an N_Procedure_Call_Statement, to a LLVM tree, which is returned.
   --  If llvm_target is non-null, this must be a function call on the RHS of a
   --  N_Assignment_Statement and the result is to be placed into that object.



   function Handled_Sequence_Of_Statements_to_llvm (gnat_node : Node_Id)
   return LLVMValueRef;
   --  Subroutine of DLC to translate gnat_node, an
   --  N_Handled_Sequence_Of_Statements, to a LLVM tree, which is returned.



   function Exception_Handler_to_llvm (gnat_node : Node_Id)
   return LLVMValueRef;
   --  Subroutine of DLC to translate gnat_node, an N_Exception_Handler,
   --  to a LLVM tree, which is returned.  This is the variant for ZCX.



   procedure Compilation_Unit_to_llvm (gnat_node : Node_Id);
   --  Subroutine of DLC to generate code for an N_Compilation unit.



   function gnat_to_llvm (gnat_node : Node_Id) return LLVMValueRef;
   --  This function is the driver of the GNAT to LLVM tree transformation
   --  process.  It is the entry point of the tree transformer.  gnat_node is
   --  the root of some LLVM tree.  Returns the root of the corresponding LLVM
   --  tree.  If this is an expression, return the LLVM equivalent of the
   --  expression.  If this is a statement, return the statement or add it to
   --  the current statement group, in which case anything returned is to be
   --  interpreted as occurring after anything added.



end DLC;
