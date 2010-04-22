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


with DracoSystem;
with PathInfo;
with System;
with Ada.IO_Exceptions;
with Interfaces.C_Streams;

package body DriverSwitch is

   NumSet      : TSwitchIndex;
   SwitchList  : TSwitchList;
   SawPG       : Boolean := False;
   SawFramePtr : Boolean := False;
   GoNoGo      : Boolean := False;
   FlagIndex   : Natural := 0;


   ---------------
   --  Proceed  --
   ---------------

   function Proceed return Boolean is
   begin
      return GoNoGo;
   end Proceed;


   -----------------------
   --  Build_Arguments  --
   -----------------------

   procedure Build_Arguments (
       source_file     : in  SU.Unbounded_String;
       compiler_flags  : out SU.Unbounded_String;
       assembler_flags : out SU.Unbounded_String
   ) is
      src_components : PathInfo.RecPathInfo;
      random_s_file  : SU.Unbounded_String;
      pipe_exists    : Boolean;
      invoke_as      : Boolean;
      S_exists       : Boolean;

      procedure Add_Dumpbase;
      procedure Add_Auxbase;
      procedure Add_Assembler_Output_File;
      procedure Tmp_Name (Buffer : System.Address);
      function  Random_Assembly_Srcfile return SU.Unbounded_String;
      procedure Add_Output_File (s_file    : in  SU.Unbounded_String;
                                 s_flag    : in  Boolean;
                                 piped     : in  Boolean;
                                 invoke_as : out Boolean);
      procedure Add_Assembler_Input_File (
                                 s_file    : in SU.Unbounded_String;
                                 piped     : in Boolean);


      pragma Import (C, Tmp_Name, "__gnat_tmp_name");

      procedure Add_Dumpbase is
         workstr : SU.Unbounded_String;
      begin
         workstr := SU.To_Unbounded_String ("-dumpbase ");
         if (src_components.extension = "adb") or
            (src_components.extension = "ads") then
            Append (workstr, src_components.filename);
         else
            Append (workstr, src_components.basename);
            Append (workstr, ".ada");
         end if;
         TackOn (compiler_flags, workstr);
      end Add_Dumpbase;

      procedure Add_Auxbase is
         workstr : SU.Unbounded_String;
      begin
         workstr := SU.To_Unbounded_String ("-auxbase ");
         Append (workstr, src_components.basename);
         TackOn (compiler_flags, workstr);
      end Add_Auxbase;

      function Random_Assembly_Srcfile return SU.Unbounded_String is
         Namelen : constant Integer := Interfaces.C_Streams.max_path_len;
         NameStr : aliased String (1 .. Namelen + 1);
      begin
         Tmp_Name (NameStr'Address);
         if NameStr (1) = ASCII.NUL then
            raise Ada.IO_Exceptions.Use_Error with
               "Invalid temporary file name";
         end if;
         return SU.To_Unbounded_String (NameStr);
      end Random_Assembly_Srcfile;

      procedure Add_Output_File (
         s_file    : in  SU.Unbounded_String;
         s_flag    : in  Boolean;
         piped     : in  Boolean;
         invoke_as : out Boolean
      ) is
         workstr : SU.Unbounded_String;
         gnatc_exists : Boolean;
         gnats_exists : Boolean;
      begin
         invoke_as := False;
         workstr   := SU.To_Unbounded_String ("-o ");
         gnatc_exists := Switch_Already_Set (SU.To_Unbounded_String ("-gnatc"),
                                             Partial => True);
         gnats_exists := Switch_Already_Set (SU.To_Unbounded_String ("-gnats"),
                                             Partial => True);

         if gnatc_exists or gnats_exists then
            Append (workstr, DracoSystem.Host_Bit_Bucket
                             (DracoSystem.Native_System.Null_File_Type));
         elsif s_flag then
            Append (workstr, src_components.basename & ".s");
         elsif piped then
            Append (workstr, "-");
            invoke_as := True;
         else
            Append (workstr, s_file);
            invoke_as := True;
         end if;
         TackOn (compiler_flags, workstr);

      end Add_Output_File;

      procedure Add_Assembler_Output_File is
         workstr : SU.Unbounded_String;
      begin
         workstr := SU.To_Unbounded_String ("-o ");
         Append (workstr, src_components.basename & ".o");
         TackOn (assembler_flags, workstr);
      end Add_Assembler_Output_File;

      procedure Add_Assembler_Input_File (
         s_file : in SU.Unbounded_String;
         piped  : in Boolean)
      is
      begin
         if piped then
            if DracoSystem.Native_System.Dash_For_Pipe then
               TackOn (assembler_flags, SU.To_Unbounded_String ("-"));
            end if;
         else
            TackOn (assembler_flags, s_file);
         end if;
      end Add_Assembler_Input_File;

   begin
      FlagIndex       := 0;
      compiler_flags  := SU.Null_Unbounded_String;
      assembler_flags := SU.Null_Unbounded_String;
      src_components  := PathInfo.Info (source_file);
      random_s_file   := Random_Assembly_Srcfile;
      S_exists        := Switch_Already_Set (SU.To_Unbounded_String ("-S"));
      pipe_exists     := Switch_Already_Set (SU.To_Unbounded_String ("-pipe"));

      Append (compiler_flags, Dump_Flags ("-I",        True));
      Append (compiler_flags, Dump_Flags ("-quiet",    False));
      Append (compiler_flags, Dump_Flags ("-nostdinc", False));
      Append (compiler_flags, Dump_Flags ("-nostdlib", False));

      Add_Dumpbase;
      Add_Auxbase;

      Append (compiler_flags, Dump_Flags ("-O",        True));
      Append (compiler_flags, Dump_Flags ("-W",        True));
      Append (compiler_flags, Dump_Flags ("-w",        False));
      Append (compiler_flags, Dump_Flags ("-p",        False));
      Append (compiler_flags, Dump_Flags ("-a",        False));
      Append (compiler_flags, Dump_Flags ("-d",        True));
      Append (compiler_flags, Dump_Flags ("-f",        True));
      Append (compiler_flags, Dump_Flags ("-g",        True));
      Append (compiler_flags, Dump_Flags ("-m",        True));
      Append (compiler_flags, Dump_Flags ("--param",   True));

--  add cc1_cpu or whatnot here (%1)

      TackOn (compiler_flags, source_file);
      Add_Output_File (random_s_file, S_exists, pipe_exists, invoke_as);
      --  We still need to set "%W" delete on fail for basename.s

      --  The assembler is a separate program with its own set of flags

      if not invoke_as then
         return;
      end if;

      FlagIndex := 0;
      TackOn (assembler_flags, SU.To_Unbounded_String ("as"));
      if DracoSystem.Native_System.Have_GNU_AS then
         Append (assembler_flags, Dump_Flags ("-v", False));
         if Switch_Already_Set (SU.To_Unbounded_String ("-w")) then
            TackOn (assembler_flags, SU.To_Unbounded_String ("-W"));
         end if;
         Append (assembler_flags, Dump_Flags ("-I", True));
      end if;

      --  put ASM_SPEC here
      --  put %Y here (Output the accumulated assembler options specified by
      --              compilations.)?  Skip?
      --  %W -o [%w]%b%0   [DONE]
      --  put ASM_FINAL_SPEC at the end

      Add_Assembler_Output_File;
      Add_Assembler_Input_File (random_s_file, pipe_exists);

   end Build_Arguments;



   ---------------------------
   -- Post_Process [Public] --
   ---------------------------

   procedure Post_Process is
      c_exists     : Boolean;
      S_exists     : Boolean;
      Q_exists     : Boolean;
   begin
      GoNoGo   := True;
      c_exists := Switch_Already_Set (SU.To_Unbounded_String ("-c"));
      S_exists := Switch_Already_Set (SU.To_Unbounded_String ("-S"));
      Q_exists := Switch_Already_Set (SU.To_Unbounded_String ("-S"));

      if SawPG and SawFramePtr then
         GoNoGo := False;
         TIO.Put_Line (switch_err_1);
      end if;

      if not c_exists and not S_exists then
         GoNoGo := False;
         TIO.Put_Line (switch_err_2);
      end if;

      if not Q_exists then
         Store_Switch (SU.To_Unbounded_String ("-quiet"));
      end if;

   end Post_Process;



   -------------------------
   -- Set_Switch [Public] --
   -------------------------

   procedure Set_Switch (Switch_Chars : in String) is
      DCS     : SU.Unbounded_String := SU.Null_Unbounded_String;
      KeyChar : Character;
   begin

      if Dedicated_Compiler_Switch (Switch_Chars) then
         DCS := SU.To_Unbounded_String ("-gnat");
         SU.Append (DCS, Switch_Chars
               (Switch_Chars'First + 5 .. Switch_Chars'Last));

         if DCS = "-gnatea" then
            Store_Switch (SU.To_Unbounded_String ("-gnatez"));
            return;
         end if;

         Store_Switch (DCS);
         return;
      end if;

      if Switch_Chars = "-k8" then
         Store_Switch (SU.To_Unbounded_String ("-gnatk8"));
         return;
      end if;

      if Switch_Chars = "-w" then
         Store_Switch (SU.To_Unbounded_String ("-gnatws"));
      end if;

      if Switch_Chars = "-Wall" then
         Store_Switch (SU.To_Unbounded_String ("-gnatwa"));
      end if;

      if Switch_Chars = "-pg" then
         Store_Switch (SU.To_Unbounded_String ("-p"));
         SawPG := True;
         return;
      end if;

      if Switch_Chars = "-fomit-frame-pointer" then
         SawFramePtr := True;
      end if;

      if Switch_Chars = "-fRTS=rtp" then
         Store_Switch (SU.To_Unbounded_String ("-mrtp"));
      end if;

      if Switch_Chars = "-fRTS=sjlj" then
         Store_Switch (SU.To_Unbounded_String ("-fsjlj"));
      end if;

      if (Switch_Chars = "-nostdinc") or
         (Switch_Chars = "-nostdlib") or
         Parameter_Switch (Switch_Chars) then
         Store_Switch (SU.To_Unbounded_String (Switch_Chars));
         return;
      end if;

      if Switch_Chars = "-coverage" then
         Store_Switch (SU.To_Unbounded_String ("-fprofile-arcs"));
         Store_Switch (SU.To_Unbounded_String ("-ftest-coverage"));
         return;
      end if;


      KeyChar := Switch_Chars (Switch_Chars'First + 1);

      --  This is for single letter switches (-w, -a, -g)
      --  There are no multiletter switches starting with these letters
      if Switch_Chars'Last = 2 then
         case KeyChar is
            when 'w' | 'a' | 'g' | 'p' | 'Q' =>
               Store_Switch (SU.To_Unbounded_String (Switch_Chars));
            when others =>
               null;
         end case;
         return;
      end if;

      --  These are multiletter switches
      case KeyChar is
         when 'O' | 'W' | 'd' | 'f' | 'I' | 'm' =>
            Store_Switch (SU.To_Unbounded_String (Switch_Chars));
         when others =>
            null;
      end case;


   end Set_Switch;


   ------------------------
   --  TackOn [Private]  --
   ------------------------

   procedure TackOn (
      collection : in out SU.Unbounded_String;
      flag       : in SU.Unbounded_String
   ) is
   begin
      if FlagIndex > 0 then
         SU.Append (collection, " ");
      end if;
      FlagIndex := FlagIndex + 1;
      SU.Append (collection, flag);
   end TackOn;




   --------------------------
   -- Dump_Flags [Private] --
   --------------------------

   function Dump_Flags (
      Switch  : in String;
      Partial : in Boolean
    ) return SU.Unbounded_String
   is
      index   : TSwitchRange := TSwitchRange'First;
      foundit : Boolean := False;
      advance : Boolean;
      result  : SU.Unbounded_String := SU.Null_Unbounded_String;
      USwitch : constant SU.Unbounded_String :=
                         SU.To_Unbounded_String (Switch);
   begin
      advance := NumSet > index;

      while advance loop
         case Partial is
            when False =>
               if USwitch = SwitchList (index) then
                  foundit := True;
                  TackOn (result, SwitchList (index));
               end if;
            when True =>
               if (Switch'Size <= SwitchList (index)'Size) and
                  (USwitch = SU.Unbounded_Slice
                     (SwitchList (index), 1, Switch'Size)) then
                  TackOn (result, SwitchList (index));
               end if;
         end case;

         exit when foundit;

         if index = TSwitchRange'Last then
            advance := False;
         else
            index := index + 1;
         end if;
      end loop;


      return result;
   end Dump_Flags;



   ----------------------------
   -- Store_Switch [Private] --
   ----------------------------

   procedure Store_Switch (Switch : in SU.Unbounded_String) is
   begin
      if (NumSet = TSwitchIndex'Last) or
         Switch_Already_Set (Switch, False) then return;
      end if;

      SwitchList (NumSet) := Switch;
      NumSet := NumSet + 1;
   end Store_Switch;



   ----------------------------------
   -- Switch_Already_Set [Private] --
   ----------------------------------

   function Switch_Already_Set (
      Switch  : in SU.Unbounded_String;
      Partial : in Boolean := False
    ) return Boolean
   is
      index   : TSwitchRange := TSwitchRange'First;
      result  : Boolean := False;
      advance : Boolean;
   begin
      advance := NumSet > index;

      while advance loop
         case Partial is
            when False =>
               if Switch = SwitchList (index) then
                  result  := True;
               end if;
            when True =>
               if (Switch'Size <= SwitchList (index)'Size) and
                  (Switch = SU.Unbounded_Slice
                     (SwitchList (index), 1, Switch'Size)) then
                  result := True;
               end if;
         end case;

         exit when result;

         if index = TSwitchRange'Last then
            advance := False;
         else
            index := index + 1;
         end if;
      end loop;

      return result;
   end Switch_Already_Set;



   -----------------------------------------
   -- Dedicated_Compiler_Switch [Private] --
   -----------------------------------------

   function Dedicated_Compiler_Switch (
      Switch_Chars : in String
   ) return Boolean is

      subtype TSubstr is String (1 .. 5);
      Strlen : constant Integer := Switch_Chars'Last;
      Substr : TSubstr;
      Result : Boolean := False;

   begin
      if Strlen < 6 then
         return Result;
      end if;

      Substr := Switch_Chars (Switch_Chars'First .. Switch_Chars'First + 4);
      if (Substr = "-gnat") or
         (Substr = "-gant") or
         (Substr = "-drac") then
         Result := True;
      end if;

      return Result;

   end Dedicated_Compiler_Switch;



   --------------------------------
   -- Parameter_Switch [Private] --
   --------------------------------

   function Parameter_Switch (Switch_Chars : in String) return Boolean is
   begin
      if Switch_Chars'Last < 7 then
         return False;
      end if;

      return Switch_Chars (Switch_Chars'First .. Switch_Chars'First + 6) =
            "--param";
   end Parameter_Switch;


   --  function Host_Bit_bucket (

end DriverSwitch;

