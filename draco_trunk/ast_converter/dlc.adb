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


with Misc;
with Decl;
with Utils01;
with Atree;    use Atree;
with Sinfo;    use Sinfo;
with Einfo;    use Einfo;
with Nlists;   use Nlists;
with Sem_Util; use Sem_Util;

package body DLC is

   ----------------------------------
   --  Draco_to_llvm_ast_converter --
   ----------------------------------

   procedure Draco_to_llvm_ast_converter (
         gnat_root               : in Node_Id;
         max_gnat_nodes          : in Node_Id;
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
      type_annotate_only : Boolean;
      TreeSync  : Utils01.TTreeAssociation :=
                  Utils01.init_gnat_to_llvm (max_gnat_nodes);
      DummySync : Utils01.TTreeAssociation :=
                  Utils01.init_gnat_to_llvm (max_gnat_nodes);
   begin
      type_annotate_only := (dlc_operating_mode = Declarations_Only);
      pragma Assert (Nkind (gnat_root) = N_Compilation_Unit,
                     "DLC: Root node is not a compilation Unit");

      --  TO-DO: void_type_node bitsize and size redefinition
      --         when type_annotate_only

      --  TO-DO: Enable GNAT Stack Checking



   end Draco_to_llvm_ast_converter;



   ---------------------------------------
   --  lvalue_required_for_attribute_p  --
   ---------------------------------------

   function lvalue_required_for_attribute_p (gnat_node : in Node_Id)
   return Boolean is
      result : Boolean;
      attrid : Attribute_Id := Get_Attribute_Id (Attribute_Name (gnat_node));
   begin
      case attrid is
         when Attribute_Pos |
              Attribute_Val |
              Attribute_Pred |
              Attribute_Succ |
              Attribute_First |
              Attribute_Last |
              Attribute_Range_Length |
              Attribute_Length |
              Attribute_Object_Size |
              Attribute_Value_Size |
              Attribute_Component_Size |
              Attribute_Max_Size_In_Storage_Elements |
              Attribute_Min |
              Attribute_Max |
              Attribute_Null_Parameter |
              Attribute_Passed_By_Reference |
              Attribute_Mechanism_Code =>
                        result := False;
         when others =>
                        result := True;
      end case;
      return result;
   end lvalue_required_for_attribute_p;



   -------------------------
   --  lvalue_required_p  --
   -------------------------

   function lvalue_required_p (gnat_node              : in Node_Id;
                               llvm_type              : in LLVMTypeRef;
                               is_constant            : in Boolean;
                               is_address_of_constant : in Boolean;
                               has_an_alias           : in Boolean)
   return Boolean is
      result      : Boolean;
      gnat_temp   : Node_Id;
      gnat_parent : Node_Id := Parent (gnat_node);
   begin
      case (Nkind (gnat_parent)) is
         when N_Reference =>

               result := True;

         when N_Attribute_Reference =>

               result := lvalue_required_for_attribute_p (gnat_parent);

         when N_Parameter_Association |
              N_Function_Call |
              N_Procedure_Call_Statement =>

               result := not is_constant
                         or else Misc.must_pass_by_ref (llvm_type)
                         or else Misc.default_pass_by_ref (llvm_type);

         when N_Indexed_Component =>

            --  Onthe the array expression can acquire an lvalue
            if Prefix (gnat_parent) /= gnat_node then

               result := False;

            else
               --  ??? Consider that referencing an indexed component with a
               --  non-constant index forces the whole aggregate to memory.
               --  Note that N_Integer_Literal is conservative, any static
               --  expression in the RM sense could probably be accepted.
               declare
                  found : Boolean := False;
               begin
                  gnat_temp := First (Expressions (gnat_parent));
                  search_non_literal :
                     loop
                        if Nkind (gnat_temp) /= N_Integer_Literal then
                           found := True;
                        end if;
                        exit search_non_literal when found;

                        gnat_temp := Next (gnat_temp);
                        exit search_non_literal when Present (gnat_temp);
                     end loop search_non_literal;
                  result := found;
               end;

               --  This mimics the "fall-through" in the GiGi code.
               --  It is a duplicate of part of the N_Slice case below.
               if not result then
                  declare
                     alias2 : Boolean := has_an_alias or else
                              Has_Aliased_Components (Etype (gnat_node));
                  begin
                     result := lvalue_required_p (
                        gnat_node              => gnat_parent,
                        llvm_type              => llvm_type,
                        is_constant            => is_constant,
                        is_address_of_constant => is_address_of_constant,
                        has_an_alias           => alias2);
                  end;
               end if;
            end if;

         when N_Slice =>

            --  Onthe the array expression can acquire an lvalue
            if Prefix (gnat_parent) /= gnat_node then

               result := False;

            else
               declare
                  alias2 : Boolean := has_an_alias or else
                           Has_Aliased_Components (Etype (gnat_node));
               begin
                  result := lvalue_required_p (
                              gnat_node              => gnat_parent,
                              llvm_type              => llvm_type,
                              is_constant            => is_constant,
                              is_address_of_constant => is_address_of_constant,
                              has_an_alias           => alias2);
               end;
            end if;

         when N_Selected_Component =>

               declare
                  alias2 : Boolean := has_an_alias or else
                           Is_Aliased (Entity (Selector_Name (gnat_node)));
               begin
                  result := lvalue_required_p (
                              gnat_node              => gnat_parent,
                              llvm_type              => llvm_type,
                              is_constant            => is_constant,
                              is_address_of_constant => is_address_of_constant,
                              has_an_alias           => alias2);
               end;

         when N_Object_Renaming_Declaration =>

               --  We need to make a real renaming only if the constant object
               --  is aliased or if we may use a renaming pointer; otherwise we
               --  can optimize and return the rvalue.  We make an exception if
               --  the object is an identifier since in this case the rvalue
               --  can be propagated to the CONST_DECL.
               --  3rd: This should match the constant case of renaming code.

               result := not is_constant
                         or else has_an_alias
                         or else Is_Composite_Type (Underlying_Type (
                                    Etype (Sinfo.Name (gnat_parent))))
                         or else Nkind (Sinfo.Name (gnat_parent)) =
                                    N_Identifier;

         when N_Object_Declaration =>

               --  We cannot use a constructor if this is an atomic object
               --  because the actual assignment might end up being done
               --  component wise.
               --  4th: We don't use a constructor if this is a class-wide
               --  object because the effective type of the object is the
               --  equivalent type of the class-wide subtype and it smashes
               --  most of the daa into an array of bytes to which we cannot
               --  convert.

               result := not is_constant
                         or else (Is_Composite_Type (Underlying_Type (
                                  Etype (gnat_node)))
                             and Is_Atomic (Defining_Entity (gnat_parent)))
                         or else Ekind (Etype (Defining_Entity (gnat_parent)))
                                 = E_Class_Wide_Subtype;

         when N_Assignment_Statement =>

               --  We cannout use a constructor if the LHS is an atomic object
               --  because the actual assignment might end up being done
               --  component-wise.

               result := not is_constant
                         or else Sinfo.Name (gnat_parent) = gnat_node
                         or else (Is_Composite_Type (Underlying_Type (
                                    Etype (gnat_node)))
                             and Is_Atomic (Entity (Sinfo.Name (
                                    gnat_parent))));

         when N_Type_Conversion |
              N_Qualified_Expression =>

               --  We must look through all conversions for composite types
               --  because we may need to bypass an intermediate conversion
               --  to a narrower record type that is generated for a formal
               --  conversion, e.g. the conversion to the root type of a
               --  hierarchy of tagged types generated for the formal
               --  conversion to the class-wide type.

            if not Is_Composite_Type (Underlying_Type (Etype (gnat_node))) then

               result := False;

            else

               --  This mimics the GiGi fall through to N_Unchecked_Type_Conv.
               result := not is_constant
                         or else lvalue_required_p (
                              gnat_node              => gnat_parent,
                              llvm_type              => Decl.get_unpadded_type
                                                         (Etype (gnat_parent)),
                              is_constant            => is_constant,
                              is_address_of_constant => is_address_of_constant,
                              has_an_alias           => has_an_alias);
            end if;

         when N_Unchecked_Type_Conversion =>

               result := not is_constant
                         or else lvalue_required_p (
                              gnat_node              => gnat_parent,
                              llvm_type              => Decl.get_unpadded_type
                                                         (Etype (gnat_parent)),
                              is_constant            => is_constant,
                              is_address_of_constant => is_address_of_constant,
                              has_an_alias           => has_an_alias);

         when N_Allocator =>

            --  We should only reach here through the N_Qualified_Expression
            --  case and, therefore, only for composite types.  Force a
            --  lvalue since a block-copy to the newly allocated area of
            --  memory is made.

               result := True;

         when N_Explicit_Dereference =>

            --  We look through dereferences for address of constant because
            --  we need to handle the special cases listed above.

            if is_constant and is_address_of_constant then

               result := lvalue_required_p (
                              gnat_node              => gnat_parent,
                              llvm_type              => Decl.get_unpadded_type
                                                         (Etype (gnat_parent)),
                              is_constant            => True,
                              is_address_of_constant => False,
                              has_an_alias           => True);

            else
               result := False;
            end if;


         when others =>

               result := False;

      end case;
      return result;
   end lvalue_required_p;



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
