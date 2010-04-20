--
--  DRACO ADA COMPILER
--  DRIVER COMPONENT
--
--
--  Copyright (c) 2010, AuroraUX (www.auroraux.org)
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


package body PipeMechanism is


   -----------------------------
   --  Create_Pipe [Private]  --
   -----------------------------
   --  This is a C-interface of the "pipe" unix command
   --  It creates a pipe, to be used prior to the first program execution
   --  It opens both ends of the pipe upon creation.

   function Create_Pipe (Pipe : not null access Pipe_Type)
   return Integer;
   pragma Import (C, Create_Pipe, "__gnat_pipe");



   ---------------------
   --  Dup [Private]  --
   ---------------------
   --  This is a C-interface of the unix command of the same name
   --  It clones an active file descriptor

   function Dup (Fd : GNAT.OS_Lib.File_Descriptor)
   return GNAT.OS_Lib.File_Descriptor;
   pragma Import (C, Dup, "__gnat_dup");



   ----------------------
   --  Dup2 [Private]  --
   ----------------------
   --  This is a C-interface of the unix command of the same name
   --  It copies one descriptor over to another

   procedure Dup2 (Old_Fd, New_Fd : GNAT.OS_Lib.File_Descriptor);
   pragma Import (C, Dup2, "__gnat_dup2");



   -----------------------
   --  Close [Private]  --
   -----------------------
   --  This is a C-interface of the file descriptor close function

   procedure Close (FD : GNAT.OS_Lib.File_Descriptor) is
      procedure C_Close (FD : GNAT.OS_Lib.File_Descriptor);
      pragma Import (C, C_Close, "close");
   begin
      C_Close (FD);
   end Close;



   ------------
   --  Pipe  --
   ------------

   procedure Pipe (
      program_1   : in String;
      arg_string1 : in String;
      program_2   : in String;
      arg_string2 : in String;
      success     : out Boolean
   ) is
      args1       : GNAT.OS_Lib.Argument_List_Access;
      args2       : GNAT.OS_Lib.Argument_List_Access;
      stdin_save  : GNAT.OS_Lib.File_Descriptor;
      datapipe    : aliased Pipe_Type;
      retcode     : integer;
   begin
      success := False;

      if Create_Pipe (datapipe'Access) /= 0 then
         return;
      end if;

      --  Duplicate STDIN so we can restore it later.
      --  This is not necessary for STDOUT since this functionality is built
      --  into the Spawn overloaded function.
      stdin_save := Dup (GNAT.OS_Lib.Standin);


      --  Spawn Program 1.  Its output will be written to the pipe
      args1 := GNAT.OS_Lib.Argument_String_To_List (arg_string1);
      GNAT.OS_Lib.Spawn (program_1, args1 (args1'Range), datapipe.Output,
                         retcode, False);
      GNAT.OS_Lib.Free (args1);
      if retcode /= 0 then
         return;
      end if;


      --  Close the write end of the pipe, and connect read end to STDIN
      Close (datapipe.Output);
      Dup2  (datapipe.Input, GNAT.OS_Lib.Standin);


      --  Spawn Program 2.  Its input will come from the output of Program 1
      args2   := GNAT.OS_Lib.Argument_String_To_List (arg_string2);
      retcode := GNAT.OS_Lib.Spawn (program_2, args2 (args2'Range));
      GNAT.OS_Lib.Free (args2);
      if retcode /= 0 then
         return;
      end if;


      --  Close the read end of the pipe, then restore STDIN
      Close (datapipe.Input);
      Dup2  (stdin_save, GNAT.OS_Lib.Standin);


      --  Close the temporary file descriptor
      Close (stdin_save);


      success := True;
   end pipe;


end PipeMechanism;

