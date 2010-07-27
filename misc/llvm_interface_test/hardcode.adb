
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

   procedure Example_Recursive is
      ret_type  : Core_h.LLVMTypeRef;
      Args      : array (1 .. 2) of Core_h.LLVMTypeRef;
      func_type : Core_h.LLVMTypeRef;
      gcd       : Core_h.LLVMValueRef; 
      NotVarArg : constant Core_h.LLVMBool := 0;
      x         : Core_h.LLVMValueRef; 
      y         : Core_h.LLVMValueRef; 
      branch1   : Core_h.LLVMValueRef;
      branch2   : Core_h.LLVMValueRef;
      xEqualsY    : Core_h.LLVMValueRef;
      xLessThanY  : Core_h.LLVMValueRef; 
      yMinusX     : Core_h.LLVMValueRef;
      ret_ret     : Core_h.LLVMValueRef;
      label_entry : Core_h.LLVMBasicBlockRef;
      label_true  : Core_h.LLVMBasicBlockRef;
      label_false : Core_h.LLVMBasicBlockRef;
      label_ret   : Core_h.LLVMBasicBlockRef;
      label_next  : Core_h.LLVMBasicBlockRef;
   begin
      ret_type  := Core_h.LLVMInt32Type;
      Args(1)   := Core_h.LLVMInt32Type;
      Args(2)   := Core_h.LLVMInt32Type;

      func_type := Core_h.LLVMFunctionType (
         ReturnType => ret_type,
         ParamTypes => Args'Address,
         ParamCount => Args'Length,
         IsVarArg   => NotVarArg   
      );

      gcd := Core_h.LLVMAddFunction ( 
         M          => module,
         Name       => Interfaces.C.Strings.New_String("gcd"),
         Functionty => func_type
      );

      x := Core_h.LLVMGetParam (gcd, 0);
      y := Core_h.LLVMGetParam (gcd, 1); 
      Core_h.LLVMSetValueName (x, Interfaces.C.Strings.New_String("x"));
      Core_h.LLVMSetValueName (y, Interfaces.C.Strings.New_String("y"));
      
      label_entry := Core_h.LLVMAppendBasicBlock (gcd, 
                        Interfaces.C.Strings.New_String("entry"));
      label_ret   := Core_h.LLVMAppendBasicBlock (gcd, 
                        Interfaces.C.Strings.New_String("return"));
      label_next  := Core_h.LLVMAppendBasicBlock (gcd, 
                        Interfaces.C.Strings.New_String("next"));
      label_true  := Core_h.LLVMAppendBasicBlock (gcd, 
                        Interfaces.C.Strings.New_String("cond_true"));
      label_false := Core_h.LLVMAppendBasicBlock (gcd, 
                        Interfaces.C.Strings.New_String("cond_false"));

      Core_h.LLVMPositionBuilderAtEnd (builder, label_entry);
      xEqualsY := Core_h.LLVMBuildICmp (
         B    => builder,
         Op   => Core_h.LLVMIntEQ,
         LHS  => x,
         RHS  => y,
         Name => Interfaces.C.Strings.New_String("tmp")
      );
      branch1 := Core_h.LLVMBuildCondBr (
         B     => builder, 
         Bif   => xEqualsY,
         Bthen => label_ret,
         Belse => label_next
      );
       
      Core_h.LLVMPositionBuilderAtEnd (builder, label_ret);
      ret_ret := Core_h.LLVMBuildRet (builder, x);
      
      Core_h.LLVMPositionBuilderAtEnd (builder, label_next);
      xLessThanY := Core_h.LLVMBuildICmp (
         B    => builder,
         Op   => Core_h.LLVMIntULT,
         LHS  => x,
         RHS  => y,
         Name => Interfaces.C.Strings.New_String("tmp")
      ); 
      branch2 := Core_h.LLVMBuildCondBr (
         B     => builder, 
         Bif   => xLessThanY,
         Bthen => label_true,
         Belse => label_false
      );
         
      Core_h.LLVMPositionBuilderAtEnd (builder, label_true);
      yMinusX := Core_h.LLVMBuildSub (
         B    => builder,
         LHS  => y,
         RHS  => x,
         Name => Interfaces.C.Strings.New_String("tmp")
      ); 
      declare
         recur_1  : Core_h.LLVMValueRef;
         Args     : array (1 .. 2) of Core_h.LLVMValueRef;
         ret_true : Core_h.LLVMValueRef;
      begin
         Args(1)   := x;
         Args(2)   := yMinusX;
         recur_1 := Core_h.LLVMBuildCall (
            B       => builder,
            Fn      => gcd,
            Args    => Args'Address,
            NumArgs => Args'Length,
            Name    => Interfaces.C.Strings.New_String("tmp")
         );
         ret_true := Core_h.LLVMBuildRet (builder, recur_1);
      end;
      
      
      Core_h.LLVMPositionBuilderAtEnd (builder, label_false);
      yMinusX := Core_h.LLVMBuildSub (
         B    => builder,
         LHS  => x,
         RHS  => y,
         Name => Interfaces.C.Strings.New_String("tmp")
      ); 
      declare
         recur_2   : Core_h.LLVMValueRef;
         Args      : array (1 .. 2) of Core_h.LLVMValueRef;
         ret_false : Core_h.LLVMValueRef;
      begin
         Args(1)   := yMinusX;
         Args(2)   := y;
         recur_2 := Core_h.LLVMBuildCall (
            B       => builder,
            Fn      => gcd,
            Args    => Args'Address,
            NumArgs => Args'Length,
            Name    => Interfaces.C.Strings.New_String("tmp")
         );
         ret_false := Core_h.LLVMBuildRet (builder, recur_2);
      end;

   end;


   function Four_Int_Function (FunctionName: String) 
      return Core_h.LLVMValueRef is
      Result : Core_h.LLVMTypeRef;
      Args   : array (1 .. 3) of Core_h.LLVMTypeRef;
      FType       : Core_h.LLVMTypeRef;
      FunctName   : Interfaces.C.Strings.chars_ptr := 
                    Interfaces.C.Strings.New_String (FunctionName);
      NotVarArg   : Core_h.LLVMBool := 0;
      FnVal       : Core_h.LLVMValueRef;
      x           : Core_h.LLVMValueRef;
      y           : Core_h.LLVMValueRef;
      z           : Core_h.LLVMValueRef;
      BasicBlock  : Core_h.LLVMBasicBlockRef;
      RetBuilder  : Core_h.LLVMValueRef;
   begin
      Result  := Core_h.LLVMInt32TypeInContext (context);
      Args(1) := Core_h.LLVMInt32TypeInContext (context);
      Args(2) := Core_h.LLVMInt32TypeInContext (context);
      Args(3) := Core_h.LLVMInt32TypeInContext (context);
      FType := Core_h.LLVMFunctionType (
         ReturnType => Result,
         ParamTypes => Args'Address,
         ParamCount => 3,
         IsVarArg   => NotVarArg   
      );
      FnVal := Core_h.LLVMAddFunction (
         M          => module,
         Name       => FunctName,
         Functionty => FType
      );
      x := Core_h.LLVMGetParam (FnVal, 0);
      y := Core_h.LLVMGetParam (FnVal, 1);
      z := Core_h.LLVMGetParam (FnVal, 2);
      Core_h.LLVMSetValueName (x, Interfaces.C.Strings.New_String ("x"));
      Core_h.LLVMSetValueName (y, Interfaces.C.Strings.New_String ("y"));
      Core_h.LLVMSetValueName (z, Interfaces.C.Strings.New_String ("z"));
      
      BasicBlock := Core_h.LLVMAppendBasicBlockInContext (
         C    => context,
         Fn   => FnVal,
         Name => Interfaces.C.Strings.New_String ("entry")
      );
      Core_h.llVMPositionBuilderAtEnd (builder, BasicBlock);
      declare
         tmp1 : Core_h.LLVMValueRef;
         tmp2 : Core_h.LLVMValueRef;
      begin
         tmp1 := Core_h.LLVMBuildBinOp (
                  B    => builder,
                  Op   => Core_h.LLVMMul,
                  LHS  => x,
                  RHS  => y,
                  Name => Interfaces.C.Strings.New_String("tmp1")
         ); 
         tmp2 := Core_h.LLVMBuildBinOp (
                  B    => builder,
                  Op   => Core_h.LLVMAdd,
                  LHS  => tmp1,
                  RHS  => z,
                  Name => Interfaces.C.Strings.New_String("tmp2")
         ); 
         RetBuilder := Core_h.LLVMBuildRet (builder, tmp2); 
      end;
      return FnVal;

   end;

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
