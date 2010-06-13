--
--  DRACO ADA COMPILER
--  DRIVER COMPONENT
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


with DracoSystem;
with DracoSystemSpecs;
with PathInfo;
with Ada.IO_Exceptions;
with Interfaces.C_Streams;

package body DriverSwitch is

   NumSet      : TSwitchIndex;
   SwitchList  : TSwitchList;
   SawPG       : Boolean := False;
   SawFramePtr : Boolean := False;
   GoNoGo      : Boolean := False;
   AutoArch    : Boolean := False;
   AutoTune    : Boolean := True;
   FlagIndex   : Natural := 0;
   DriverCom   : RecDriverCommands;
   Ass_options : SU.Unbounded_String := SU.Null_Unbounded_String;


   -------------------------------------------
   --  Append_Binary_Search_Path [Private]  --
   -------------------------------------------

   procedure Append_Binary_Search_Path (Switch_Chars : in String) is
   begin
      if Switch_Chars'Length > 3 then
         if DriverCom.binsearch /= SU.Null_Unbounded_String then
            Append (DriverCom.binsearch, ":");
         end if;
         Append (DriverCom.binsearch, Switch_Chars
                (Switch_Chars'First + 3 .. Switch_Chars'Last));
      end if;
   end Append_Binary_Search_Path;

   ----------------------------------
   --  Detect_Local_CPU [Private]  --
   ----------------------------------

   function Detect_Local_CPU (DetectMethod : in TDetectMethod)
   return SU.Unbounded_String is

      --  Argv is really type char_ptr_array, but theres always only one input
      --  so we can interchangable use the char_ptr instead

--      function Detect_Local_CPU (
--         argc : Interfaces.C.int;
--         argv : Interfaces.C.Strings.chars_ptr)
--      return Interfaces.C.Strings.chars_ptr;
--      pragma Import (C, Detect_Local_CPU, "host_detect_local_cpu");

--      numArguments : constant Interfaces.C.int := 1;
--      March        : constant String := "arch";
--      Mtune        : constant String := "tune";
--      PMarch       : constant Interfaces.C.Strings.chars_ptr :=
--                              Interfaces.C.Strings.New_String (March);
--      PMtune       : constant Interfaces.C.Strings.chars_ptr :=
--                              Interfaces.C.Strings.New_String (Mtune);
--      Response     : String (1 .. 128);
--      Presponse    : Interfaces.C.Strings.chars_ptr;
   begin
--      case DetectMethod is
--         when ARCH => Presponse := Detect_Local_CPU (numArguments, PMarch);
--         when TUNE => Presponse := Detect_Local_CPU (numArguments, PMtune);
--      end case;
--      Response := Interfaces.C.Strings.Value (Presponse);
--      return SU.To_Unbounded_String (Response);

--  The host_detect_local_cpu has tons of recursive header files to pull in
--  and would eventually need to be converted purely to ada, but since this
--  work is planned to be removed over time, it's not worth it.  For now
--  just return a blank.

      if DetectMethod = ARCH then
         return SU.To_Unbounded_String ("delete-me-soon");
      end if;

      return SU.Null_Unbounded_String;

   end Detect_Local_CPU;


   ---------------
   --  Proceed  --
   ---------------

   function Proceed return Boolean is
   begin
      return GoNoGo;
   end Proceed;


   ----------------
   --  Commands  --
   ----------------

   function Commands return RecDriverCommands is
   begin
      return DriverCom;
   end Commands;


   -----------------------
   --  Build_Arguments  --
   -----------------------

   procedure Build_Arguments (
      source_file     : in  SU.Unbounded_String;
      compiler_flags  : out SU.Unbounded_String;
      assembler_flags : out SU.Unbounded_String;
      temporary_file  : out SU.Unbounded_String
   ) is
      src_components : PathInfo.RecPathInfo;
      random_s_file  : SU.Unbounded_String;
      invoke_as      : Boolean;
      S_exists       : Boolean;

      procedure Add_Dumpbase;
      procedure Add_Auxbase;
      procedure Add_Assembler_Output_File;
      function  Random_Assembly_Srcfile return SU.Unbounded_String;
      procedure Add_Output_File (s_file    : in  SU.Unbounded_String;
                                 s_flag    : in  Boolean;
                                 piped     : in  Boolean;
                                 invoke_as : out Boolean);
      procedure Add_Assembler_Input_File (
                                 s_file    : in SU.Unbounded_String;
                                 piped     : in Boolean);


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
--         TackOn (compiler_flags, workstr);
      end Add_Auxbase;

      function Random_Assembly_Srcfile return SU.Unbounded_String is
         maxlen : constant Positive := Interfaces.C_Streams.max_path_len + 1;
         subtype TTemporary is String (1 .. maxlen);
         type name_access is access all TTemporary;
         NameStr : aliased TTemporary;
         NameStr_access : name_access;
         result : SU.Unbounded_String := SU.Null_Unbounded_String;

         procedure Tmp_Name (tmp_filename : name_access);
         pragma Import (C, Tmp_Name, "__gnat_tmp_name");

      begin
         NameStr := (others => ' ');
         NameStr_access := NameStr'Access;
         Tmp_Name (NameStr_access);

         if NameStr (1) = ASCII.NUL then
               raise Ada.IO_Exceptions.Use_Error with
                  "Invalid temporary file name";
         end if;

         declare
            k : Natural := 1;
         begin
            loop
               exit when (NameStr (k) = ASCII.NUL) or (k = maxlen);
               k := k + 1;
            end loop;
            result := SU.To_Unbounded_String (NameStr (1 .. k - 1) & ".s");
         end;

         return result;
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
         gnatc_exists := Switch_Already_Set ("-gnatc", Partial => True);
         gnats_exists := Switch_Already_Set ("-gnats", Partial => True);

         if gnatc_exists or gnats_exists then
            Append (workstr, DracoSystem.Host_Bit_Bucket
                             (DracoSystemSpecs.Native_System.Null_File_Type));
         elsif s_flag then
            Append (workstr, src_components.basename & ".s");
         elsif piped then
            Append (workstr, "-");
            invoke_as := True;
         else
            Append (workstr, s_file);
            temporary_file := s_file;
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
            if DracoSystemSpecs.Native_System.Dash_For_Pipe then
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
      temporary_file  := SU.Null_Unbounded_String;
      src_components  := PathInfo.Info (source_file);
      random_s_file   := Random_Assembly_Srcfile;
      S_exists        := Switch_Already_Set ("-S");

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

      Add_CC_Flags_Per_Target_Spec (compiler_flags);

      TackOn (compiler_flags, source_file);
      Add_Output_File (random_s_file, S_exists, DriverCom.pipe, invoke_as);
      --  We still need to set "%W" delete on fail for basename.s

      --  The assembler is a separate program with its own set of flags

      if not invoke_as then
         return;
      end if;

      FlagIndex := 0;
      --  TackOn (assembler_flags, SU.To_Unbounded_String ("as"));
      if DracoSystemSpecs.Native_System.Have_GNU_AS then
         Append (assembler_flags, Dump_Flags ("-v", False));
         if Switch_Already_Set ("-w") then
            TackOn (assembler_flags, SU.To_Unbounded_String ("-W"));
         end if;
         Append (assembler_flags, Dump_Flags ("-I", True));
      end if;

      --  put ASM_SPEC here [skipped since DLC incorporation will delete]

      if SU.Length (Ass_options) > 0 then
         TackOn (assembler_flags, Ass_options);
      end if;

      --  put ASM_FINAL_SPEC at the end (only applies to alpha/osf5 [skipped]

      Add_Assembler_Output_File;
      Add_Assembler_Input_File (random_s_file, DriverCom.pipe);

   end Build_Arguments;



   ---------------------------
   -- Post_Process [Public] --
   ---------------------------

   procedure Post_Process is
      c_exists     : Boolean;
      S_exists     : Boolean;
      Q_exists     : Boolean;

      use type DracoSystem.CC1_SPEC;
   begin
      GoNoGo   := True;
      c_exists := Switch_Already_Set ("-c");
      S_exists := Switch_Already_Set ("-S");
      Q_exists := Switch_Already_Set ("-Q");

      if SawPG and SawFramePtr then
         GoNoGo := False;
         TIO.Put_Line (switch_err_1);
      end if;

      if not c_exists and not S_exists then
         GoNoGo := False;
         TIO.Put_Line (switch_err_2);
      end if;

      if not Q_exists then
         Store_Switch ("-quiet");
      end if;

      --  This section will be removed after DLC replaces GiGi
      if (DracoSystemSpecs.Native_System.CC_Flags = DracoSystem.stdmips) or
         (DracoSystemSpecs.Native_System.CC_Flags = DracoSystem.mipsiris) then

         if Switch_Already_Set ("-mel") and
            Switch_Already_Set ("-meb") then
            GoNoGo := False;
            TIO.Put_Line ("The -EB and -EL switches can't be used together");
         end if;

      end if;

   end Post_Process;



   -------------------------
   -- Set_Switch [Public] --
   -------------------------

   procedure Set_Switch (Switch_Chars : in String) is
      DCS     : SU.Unbounded_String := SU.Null_Unbounded_String;
      KeyChar : Character;
      workstr : String (1 .. 32);

      use type DracoSystem.CC1_SPEC;
   begin

      --  All the --xxx long forms have already been converted to their
      --  short form except for --param by this point.
      if Switch_Chars = "-fversion" then
         DriverCom.version := True;
         return;
      elsif Switch_Chars = "-v" then
         DriverCom.verbose := True;
      elsif Switch_Chars = "-fhelp" then
         DriverCom.help := True;
         return;
      elsif Switch_Chars = "-dumpmachine" then
         DriverCom.dumpmachine := True;
         return;
      elsif Switch_Chars = "-dumpversion" then
         DriverCom.dumpversion := True;
         return;
      elsif Switch_Chars = "-save-temps" then
         DriverCom.save_temps := True;
         return;
      elsif Switch_Chars = "-print-search-dirs" then
         DriverCom.psearchdir := True;
         return;
      elsif Switch_Chars = "-pipe" then
         DriverCom.pipe := True;
         return;
      end if;



      if Dedicated_Compiler_Switch (Switch_Chars) then
         DCS := SU.To_Unbounded_String ("-gnat");
         SU.Append (DCS, Switch_Chars
               (Switch_Chars'First + 5 .. Switch_Chars'Last));

         if DCS = "-gnatea" then
            Store_Switch ("-gnatez");
            return;
         end if;

         Store_Switch (SU.To_String (DCS));
         return;
      end if;

      if Switch_Chars = "-k8" then
         Store_Switch ("-gnatk8");
         return;
      end if;

      if Switch_Chars = "-w" then
         Store_Switch ("-gnatws");
      end if;

      if Switch_Chars = "-Wall" then
         Store_Switch ("-gnatwa");
      end if;

      if Switch_Chars = "-pg" then
         Store_Switch ("-p");
         SawPG := True;
         return;
      end if;

      if Switch_Chars = "-fomit-frame-pointer" then
         SawFramePtr := True;
      end if;

      if Switch_Chars = "-fRTS=rtp" then
         Store_Switch ("-mrtp");
      end if;

      if (Switch_Chars = "-nostdinc") or
         (Switch_Chars = "-nostdlib") or
         Parameter_Switch (Switch_Chars) then
         Store_Switch (Switch_Chars);
         return;
      end if;

      if Switch_Chars = "-coverage" then
         Store_Switch ("-fprofile-arcs");
         Store_Switch ("-ftest-coverage");
         return;
      end if;

      if Switch_Chars'Last >= 4 and then
        (Switch_Chars (Switch_Chars'First ..
                       Switch_Chars'First + 3) = "-Wa,") then
         SU.Append (Ass_options, SU.To_Unbounded_String (
             Switch_Chars (Switch_Chars'First + 4 .. Switch_Chars'Last)
             & " "));
         for n in Positive range 1 .. SU.Length (Ass_options) loop
            if SU.Element (Ass_options, n) = ',' then
               SU.Replace_Element (Ass_options, n, ' ');
            end if;
         end loop;
         return;
      end if;

      if Switch_Chars'Last >= 12 and then
        (Switch_Chars (Switch_Chars'First ..
                       Switch_Chars'First + 11) = "-Xassembler ") then
         SU.Append (Ass_options, SU.To_Unbounded_String (
             Switch_Chars (Switch_Chars'First + 12 .. Switch_Chars'Last)
             & " "));
         return;
      end if;

      --  This section will be removed after DLC replaces GiGi
      if (DracoSystemSpecs.Native_System.CC_Flags = DracoSystem.i386) or
         (DracoSystemSpecs.Native_System.CC_Flags = DracoSystem.darwin) or
         (DracoSystemSpecs.Native_System.CC_Flags = DracoSystem.x86linux) then

         if Switch_Chars = "-mintel-syntax" then
            Store_Switch ("-masm=intel");
            TIO.Put_Line ("'-mintel-syntax' is deprecated. " &
                          "Use '-masm=intel' instead.");
            return;
         end if;
         if Switch_Chars = "-mno-intel-syntax" then
            Store_Switch ("-masm=att");
            TIO.Put_Line ("'-mno-intel-syntax' is deprecated. " &
                          "Use '-masm=att' instead.");
            return;
         end if;
         if Switch_Chars = "-msse5" then
            Store_Switch ("-mavx");
            TIO.Put_Line ("'-msse5' was removed.");
            return;
         end if;
         if (Switch_Chars'Last >= 7) and then
            (Switch_Chars (Switch_Chars'First ..
                           Switch_Chars'First + 5) = "-mcpu=") then
            workstr := Switch_Chars (Switch_Chars'First + 6 ..
                                     Switch_Chars'Last);
            Store_Switch ("-mtune=" & workstr);
            TIO.Put_Line ("'-mcpu=" & workstr & "' is deprecated. Use " &
                   "'-mtune=" & workstr & "' or '-march=" & workstr &
                   "' instead.");
            return;
         end if;
         if Switch_Chars = "-march=native" then
            AutoArch := True;
            return;
         end if;
         if (Switch_Chars'Last >= 7) and then
            (Switch_Chars (Switch_Chars'First ..
                           Switch_Chars'First + 6) = "-mtune=") and then
            (Switch_Chars /= "-mtune=native") then
            AutoTune := False;
            return;
         end if;
      end if;

      --  This section will be removed after DLC replaces GiGi
      if (DracoSystemSpecs.Native_System.CC_Flags = DracoSystem.stdmips) or
         (DracoSystemSpecs.Native_System.CC_Flags = DracoSystem.mipsiris) then

         if Switch_Chars = "-EB" then
            Store_Switch ("-meb");
            return;
         end if;
         if Switch_Chars = "-EL" then
            Store_Switch ("-mel");
            return;
         end if;
         if (DracoSystemSpecs.Native_System.CC_Flags = DracoSystem.mipsiris)
            and Switch_Chars = "-static" then
            Store_Switch ("-mno-abicalls");
            return;
         end if;

      end if;


      KeyChar := Switch_Chars (Switch_Chars'First + 1);

      --  This is for single letter switches (-w, -a, -g)
      --  There are no multiletter switches starting with these letters
      if Switch_Chars'Last = 2 then
         case KeyChar is
            when 'w' | 'a' | 'g' | 'p' | 'Q' | 'c' | 'S' =>
               Store_Switch (Switch_Chars);
            when others =>
               null;
         end case;
         return;
      end if;

      --  These are multiletter switches
      --  The second row is for CC1_SPEC specific switches
      case KeyChar is
         when 'O' | 'W' | 'd' | 'f' | 'I' | 'm' =>
            Store_Switch (Switch_Chars);
         when 'G' | 't' | 's' =>
            Store_Switch (Switch_Chars);
         when others =>
            null;
      end case;

      if KeyChar = 'B' then
         Append_Binary_Search_Path (Switch_Chars);
      end if;


   end Set_Switch;


   procedure Add_CC_Flags_Per_Target_Spec (
      collection : in out SU.Unbounded_String
   ) is

      procedure i386_core;

      procedure i386_core is
      begin
         if DracoSystemSpecs.Native_System.CPU_AutoDetect then
            if AutoArch then
               TackOn (collection, Detect_Local_CPU (ARCH));
            end if;
            if AutoTune then
               TackOn (collection, Detect_Local_CPU (TUNE));
            end if;
         end if;
      end i386_core;

   begin
      case DracoSystemSpecs.Native_System.CC_Flags is
      when DracoSystem.g_star =>

         Append (collection, Dump_Flags ("-G", True));

      when DracoSystem.android =>

         if Switch_Already_Set ("-mandroid") and
            then not Switch_Already_Set ("-fexceptions") then
               TackOn (collection, "-fno-exceptions");
         end if;

      when DracoSystem.symbian =>

         if not Switch_Already_Set ("-fbuiltin") and
            then not Switch_Already_Set ("-fno-builtin") then
               TackOn (collection, "-fno-builtin");
         end if;
         if not Switch_Already_Set ("-fvisibility=", Partial => True) then
               TackOn (collection, "-fvisibility=hidden");
         end if;
         if not Switch_Already_Set ("-fshort-enums") and
            then not Switch_Already_Set ("-fno-short-enums") then
               TackOn (collection, "-fno-short-enums");
         end if;
         if not Switch_Already_Set ("-fshort-wchar") and
            then not Switch_Already_Set ("-fno-short-wchar") then
               TackOn (collection, "-fshort-wchar");
         end if;

      when DracoSystem.vxworks =>

         if Switch_Already_Set ("-tstrongarm") then
               TackOn (collection, "-mlittle-endian -mcpu-strongarm");
         elsif Switch_Already_Set ("-t4") then
               TackOn (collection, "-mlittle-endian -march=armv4");
         elsif Switch_Already_Set ("-t4be") then
               TackOn (collection, "-mbig-endian -march=armv4");
         elsif Switch_Already_Set ("-t4t") then
               TackOn (collection, "-mthumb -mthumb-interwork " &
                                   "-mlittle-endian -march=armv4t");
         elsif Switch_Already_Set ("-t4tbe") then
               TackOn (collection, "-mthumb -mthumb-interwork " &
                                   "-mbig-endian -march=armv4t");
         elsif Switch_Already_Set ("-t5") then
               TackOn (collection, "-mlittle-endian -march=armv5");
         elsif Switch_Already_Set ("-t5be") then
               TackOn (collection, "-mbig-endian -march=armv5");
         elsif Switch_Already_Set ("-t5t") then
               TackOn (collection, "-mthumb -mthumb-interwork " &
                                   "-mlittle-endian -march=armv5");
         elsif Switch_Already_Set ("-t5tbe") then
               TackOn (collection, "-mthumb -mthumb-interwork " &
                                   "-mbig-endian -march=armv5");
         elsif Switch_Already_Set ("-txscale") then
               TackOn (collection, "-mlittle-endian -mcpu=xscale");
         elsif Switch_Already_Set ("-txscalebe") then
               TackOn (collection, "-mbig-endian -mcpu=xscale");
         else
               TackOn (collection, "-march=armv4");
         end if;

      when DracoSystem.i386 =>

         i386_core;

      when DracoSystem.x86linux =>

         i386_core;
         Append (collection, Dump_Flags ("-G", True));

      when DracoSystem.darwin =>

         i386_core;
         if not Switch_Already_Set ("-mkernel") and then
            not Switch_Already_Set ("-static") and then
            not Switch_Already_Set  ("-mdynamic-no-pic") then
            TackOn (collection, "-fPIC");
         end if;
         if Switch_Already_Set ("g") and then
            not Switch_Already_Set ("-fno-eliminate-unused-debug-symbols") then
            TackOn (collection, "-feliminate-unused-debug-symbols");
         end if;

      when DracoSystem.stdmips | DracoSystem.mipsiris =>

         if Switch_Already_Set ("-gline") and then
            not Switch_Already_Set ("-g") and then
            not Switch_Already_Set ("-g0") and then
            not Switch_Already_Set ("-g1") and then
            not Switch_Already_Set  ("-g2") then
            TackOn (collection, "-g1");
         end if;
         Append (collection, Dump_Flags ("-G", True));

      when others =>
         null;
      end case;
   end Add_CC_Flags_Per_Target_Spec;



   ------------------------
   --  TackOn [Private]  --
   ------------------------

   procedure TackOn (
      collection : in out SU.Unbounded_String;
      flag       : in SU.Unbounded_String
   ) is
   begin
      if flag = SU.Null_Unbounded_String then
         return;
      end if;

      if FlagIndex > 0 then
         SU.Append (collection, " ");
      end if;
      FlagIndex := FlagIndex + 1;
      SU.Append (collection, flag);
   end TackOn;

   procedure TackOn (
      collection : in out SU.Unbounded_String;
      flag       : in String
   ) is
   begin
      if flag'Last = 0 then
         return;
      end if;

      TackOn (collection, SU.To_Unbounded_String (flag));
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
      advance : Boolean := NumSet > index;
      result  : SU.Unbounded_String := SU.Null_Unbounded_String;
      USwitch : constant SU.Unbounded_String :=
                         SU.To_Unbounded_String (Switch);
   begin
      while advance loop
         case Partial is
            when False =>
               if USwitch = SwitchList (index) then
                  foundit := True;
                  TackOn (result, SwitchList (index));
               end if;
            when True =>
               if (Switch'Last <= SU.Length (SwitchList (index))) and then
                  (USwitch = SU.Unbounded_Slice
                     (SwitchList (index), 1, Switch'Last)) then
                  TackOn (result, SwitchList (index));
               end if;
         end case;

         exit when foundit;

         if index = TSwitchRange'Last then
            advance := False;
         else
            index := index + 1;
            advance := NumSet > index;
         end if;
      end loop;


      return result;
   end Dump_Flags;



   ----------------------------
   -- Store_Switch [Private] --
   ----------------------------

   procedure Store_Switch (SSwitch : in String) is
      Switch  : constant SU.Unbounded_String :=
                         SU.To_Unbounded_String (SSwitch);
   begin
      if (NumSet = TSwitchIndex'Last) or
         Switch_Already_Set (SSwitch) then return;
      end if;

      SwitchList (NumSet) := Switch;
      NumSet := NumSet + 1;
   end Store_Switch;



   ----------------------------------
   -- Switch_Already_Set [Private] --
   ----------------------------------

   function Switch_Already_Set (
      SSwitch : in String;
      Partial : in Boolean := False
    ) return Boolean
   is
      Switch  : constant SU.Unbounded_String :=
                         SU.To_Unbounded_String (SSwitch);
      index   : TSwitchRange := TSwitchRange'First;
      result  : Boolean := False;
      advance : Boolean := NumSet > index;
   begin
      while advance loop
         case Partial is
            when False =>
               if Switch = SwitchList (index) then
                  result := True;
               end if;
            when True =>
               if (SU.Length (Switch) <= SU.Length (SwitchList (index)))
                  and then (Switch = SU.Unbounded_Slice
                     (SwitchList (index), 1, SU.Length (Switch))) then
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
         return False;
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

