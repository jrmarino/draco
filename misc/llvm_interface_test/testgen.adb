--  #################
--    Hardcode MAIN
--  #################


with Hardcode;
with Interfaces.C;
with Core_h;
with Ada.Command_Line;
with Ada.Text_IO; Use Ada.Text_IO;

procedure Testgen is
   menu_option: Natural := 0;
begin
   if Ada.Command_Line.Argument_Count > 0 then 
      declare
         argstring : String := Ada.Command_Line.Argument (1);
      begin
         menu_option := Natural'Value (argstring);
      end;
   end if;

   Hardcode.Create_Context;
   case menu_option is
      when 1 => 
         declare
            LVal   : Interfaces.C.double := 4.0;
            RVal   : Interfaces.C.double := 5.0;
            LRef   : Core_h.LLVMValueRef;
            RRef   : Core_h.LLVMValueRef;
            Result : Core_h.LLVMValueRef;
         begin
            Put_Line ("================================");
            Put_Line ("Option 1:  Define constant 4 + 5");
            Put_Line ("================================");
            LRef   := Hardcode.Codegen_numberExprAST (LVal);
            RRef   := Hardcode.Codegen_numberExprAST (RVal);
            Result := Hardcode.Codegen_binaryExprAST (
                        LRef, 
                        RRef, 
                        Core_h.LLVMFAdd,
                        "x"
            );
            Hardcode.Dump_Value (Result);
         end;
      when 2 => 
         declare
            LVal        : Interfaces.C.double := 4.0;
            RVal        : Interfaces.C.double := 5.0;
            LRef        : Core_h.LLVMValueRef;
            RRef        : Core_h.LLVMValueRef;
            TheFunction : Core_h.LLVMValueRef;
            Result      : Core_h.LLVMValueRef;
         begin
            Put_Line ("==================================");
            Put_Line ("Option 1:  Define expression 4 + 5");
            Put_Line ("==================================");
            LRef        := Hardcode.Codegen_numberExprAST (LVal);
            RRef        := Hardcode.Codegen_numberExprAST (RVal);
            Result      := Hardcode.Codegen_binaryExprAST (
                           LHS    => LRef, 
                           RHS    => RRef, 
                           opcode => Core_h.LLVMFAdd,
                           name   => "x"
            );
            TheFunction := Hardcode.Codegen_anonymous_function_double;
            Hardcode.Codegen_SetInsertPoint ("Entry", TheFunction, Result);
         end;
      when  3 => Put_Line (" 3"); 
         
      when others => 
         declare 
            testval : Interfaces.C.double := 7.0;
            Result  : Core_h.LLVMValueRef;
         begin
            Put_Line ("=================================");
            Put_Line ("Option 0:  Define float value 7.0");
            Put_Line ("=================================");
            Result := Hardcode.Codegen_numberExprAST (testval);
            Hardcode.Dump_Value (Result);
         end;
   end case;
   Hardcode.Dump_Module;
   Put_Line ("");

end Testgen;
