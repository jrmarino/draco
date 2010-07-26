
--  #####################
--    Hardcode codegen
--  #####################

with System.Address_To_Access_Conversions;

package body Hardcode is

   procedure Codegen_SetInsertPoint (
         PointName   : in String;
         TheFunction : in Core_h.LLVMValueRef;
         Result      : in Core_h.LLVMValueRef
   ) is
      PointChars : Interfaces.C.Strings.chars_ptr := 
                    Interfaces.C.Strings.New_String (PointName);
      BasicBlock : Core_h.LLVMBasicBlockRef;
      RetBuilder : Core_h.LLVMValueRef;
   begin
      BasicBlock := Core_h.LLVMAppendBasicBlockInContext (
         C    => context,
         Fn   => TheFunction,
         Name => PointChars
      );
      Core_h.llVMPositionBuilderAtEnd (builder, BasicBlock);
      RetBuilder := Core_h.LLVMBuildRet (builder, Result); 
   end Codegen_SetInsertPoint;


   function Codegen_anonymous_function_double 
      return Core_h.LLVMValueRef is
      DoubleType  : Core_h.LLVMTypeRef;
      FType       : Core_h.LLVMTypeRef;
      FunctName   : Interfaces.C.Strings.chars_ptr := 
                    Interfaces.C.Strings.New_String ("");
      PtrPTypes   : System.Address := System.Null_Address;
      NotVarArg   : Core_h.LLVMBool := 0;
   begin
      DoubleType  := Core_h.LLVMDoubleTypeInContext (context);
      FType := Core_h.LLVMFunctionType (
         ReturnType => DoubleType,
         ParamTypes => PtrPTypes,
         ParamCount => 0,
         IsVarArg   => NotVarArg   
      );
      return Core_h.LLVMAddFunction (
         M          => module,
         Name       => FunctName,
         Functionty => FType
      );
      
   end Codegen_anonymous_function_double;

   function Codegen_numberExprAST (Val : in Interfaces.C.double)
      return Core_h.LLVMValueRef is
      DTypeRef  : Core_h.LLVMTypeRef;
      DValueRef : Core_h.LLVMValueRef;
   begin
      DTypeRef  := Core_h.LLVMDoubleTypeInContext (context);
      DValueRef := Core_h.LLVMConstReal (DTypeRef, Val);
      return Core_h.LLVMIsAConstantFP (DValueRef);
   end Codegen_numberExprAST;


   procedure Create_Context is
      modname : Interfaces.C.Strings.chars_ptr := 
              Interfaces.C.Strings.New_String ("Draco-LLVM Converter");
   begin
      context := Core_h.LLVMContextCreate;
  --  builder := Core_h.LLVMCreateBuilderInContext (context);
      builder := Core_h.LLVMCreateBuilder;
      module  := Core_h.LLVMModuleCreateWithNameInContext (
                  modname,
                  context 
      );
   end  Create_Context;


   procedure Dump_Value (DValueRef : in Core_h.LLVMValueRef) is
   begin
      Core_h.LLVMDumpValue (DValueRef);
   end Dump_Value;
   
   
   procedure Dump_Module is
   begin
      Core_h.LLVMDumpModule (module);
   end;


   function Codegen_binaryExprAST (
      LHS    : Core_h.LLVMValueRef;
      RHS    : Core_h.LLVMValueRef;
      opcode : Core_h.LLVMOpcode;
      name   : String
   ) return Core_h.LLVMValueRef is
      pName : Interfaces.C.Strings.chars_ptr := 
              Interfaces.C.Strings.New_String (name);
   begin
      return Core_h.LLVMBuildBinOp (builder, opcode, LHS, RHS, pName);      
   end;





end Hardcode;
