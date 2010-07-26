
--  #####################
--    Hardcode codegen
--  #####################


with Core_h;
with Interfaces.C.Strings;

package Hardcode is

   function Codegen_numberExprAST (Val : in Interfaces.C.double)
      return Core_h.LLVMValueRef;
   function Codegen_binaryExprAST (
      LHS    : Core_h.LLVMValueRef;
      RHS    : Core_h.LLVMValueRef;
      opcode : Core_h.LLVMOpcode;
      name   : String
   ) return Core_h.LLVMValueRef;
   function Codegen_anonymous_function_double 
      return Core_h.LLVMValueRef;
   procedure Codegen_SetInsertPoint (
         PointName   : in String;
         TheFunction : in Core_h.LLVMValueRef;
         Result      : in Core_h.LLVMValueRef
   );
   procedure Create_Context;
   procedure Dump_Value (DValueRef : in Core_h.LLVMValueRef);
   procedure Dump_Module;

private

   context : Core_h.LLVMContextRef;
   builder : Core_h.LLVMBuilderRef;
   module  : Core_h.LLVMModuleRef;

end Hardcode;
