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


with SwitchMap;
with DriverSwitch;
with Commands;
with Ada.Strings.Unbounded;

--  Delete later
with Ada.Text_IO;

procedure Draco is

   package SU renames Ada.Strings.Unbounded;

   DriverCom       : DriverSwitch.RecDriverCommands;
   ProcessFiles    : Boolean := True;
   Did_Something   : Boolean := False;
begin
   Commands.Initialize_Paths;
   SwitchMap.Analyze_Command_Line;
   if SwitchMap.File_Count = 0 then
      ProcessFiles := False;
   end if;

   declare
      GroupedSwitches : constant SwitchMap.TGroupedSwitches :=
                        SwitchMap.Get_Switch_List;
   begin
      for n in Positive range GroupedSwitches'First ..
                              GroupedSwitches'Last loop
         DriverSwitch.Set_Switch (SU.To_String (GroupedSwitches (n)));
      end loop;
   end;

   DriverCom := DriverSwitch.Commands;

   if DriverCom.dumpmachine then
      Did_Something := True;
      ProcessFiles  := False;
      Commands.Dump_Machine;
   end if;
   if DriverCom.dumpversion then
      Did_Something := True;
      ProcessFiles  := False;
      Commands.Dump_Version;
   end if;
   if DriverCom.psearchdir then
      Did_Something := True;
      ProcessFiles  := False;
      Commands.Print_Search_Dirs;
   end if;

   if ProcessFiles then
      DriverSwitch.Post_Process;
      if DriverSwitch.Proceed then
         declare
            FileList : constant SwitchMap.TFileList :=
                       SwitchMap.Get_File_List;
            compiler_flags  : SU.Unbounded_String;
            assembler_flags : SU.Unbounded_String;
         begin
            for n in Positive range FileList'First .. FileList'Last loop
               DriverSwitch.Build_Arguments (
                  source_file    => FileList (n),
                  compiler_flags => compiler_flags,
                  assembler_flags => assembler_flags
               );
               Ada.Text_IO.Put_Line ("gnat1: " & SU.To_String (compiler_flags));
               Ada.Text_IO.Put_Line ("asm  : " & SU.To_String (assembler_flags));
            end loop;
         end;
      end if;
   elsif not Did_Something then
      Commands.Display_Error (Commands.NoInputFiles);
   end if;

end Draco;

