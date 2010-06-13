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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Directories;
with PipeMechanism;
with GNAT.OS_Lib;

procedure Draco is

   package SU renames Ada.Strings.Unbounded;
   package TIO renames Ada.Text_IO;

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

   if DriverCom.binsearch /= SU.Null_Unbounded_String then
      Commands.Override_Default_Search_Path (
         SU.To_String (DriverCom.binsearch)
      );
   end if;
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
            temporary_file  : SU.Unbounded_String;
            gnat1_fullpath  : constant String := Commands.Complete_Gnat1_Path;
            assy_fullpath   : constant String := Commands.Full_Assembler_Path;
            pipe_success    : Boolean := True;
            spawn_success   : Boolean := True;
            temp_created    : Boolean := False;
         begin
            if gnat1_fullpath'Length = 0 then
               Commands.Display_Error (Commands.GNAT1_Not_Found);
            else
               if DriverCom.verbose then
                  TIO.Put_Line ("COMPILER_PATH=" & gnat1_fullpath);
                  TIO.Put_Line ("ASSEMBLR_PATH=" & assy_fullpath);
               end if;

               for n in Positive range FileList'First .. FileList'Last loop
                  DriverSwitch.Build_Arguments (
                     source_file     => FileList (n),
                     compiler_flags  => compiler_flags,
                     assembler_flags => assembler_flags,
                     temporary_file  => temporary_file
                  );
                  declare
                     str_compiler_flags  : constant String :=
                                           SU.To_String (compiler_flags);
                     str_assembler_flags : constant String :=
                                           SU.To_String (assembler_flags);
                     str_temp_file       : constant String :=
                                           SU.To_String (temporary_file);
                  begin
                     if DriverCom.pipe then
                        if pipe_success then

                           if DriverCom.verbose then
                              TIO.Put_Line ("{DRAC} " & str_compiler_flags &
                                         " | {ASSY} " & str_assembler_flags);
                           end if;
                           PipeMechanism.Pipe (
                              gnat1_fullpath,
                              str_compiler_flags,
                              assy_fullpath,
                              str_assembler_flags,
                              pipe_success
                           );
                        end if;
                     else
                        if spawn_success then
                           declare
                              args1 : GNAT.OS_Lib.Argument_List_Access;
                           begin
                              args1 := GNAT.OS_Lib.Argument_String_To_List
                                       (str_compiler_flags);
                              if DriverCom.verbose then
                                 TIO.Put_Line (
                                    "{DRAC} " &
                                    SU.To_String (compiler_flags)
                                 );
                              end if;
                              GNAT.OS_Lib.Spawn (
                                 Program_Name => gnat1_fullpath,
                                 Args         => args1 (args1'Range),
                                 Success      => spawn_success);
                              GNAT.OS_Lib.Free (args1);
                              if (str_temp_file'Length > 0) then
                                 temp_created := Ada.Directories.Exists
                                                 (str_temp_file);
                              end if;
                           end;
                        end if;
                        if spawn_success and then
                           (str_assembler_flags'Length > 0) then
                           declare
                              args2 : GNAT.OS_Lib.Argument_List_Access;
                           begin
                              args2 := GNAT.OS_Lib.Argument_String_To_List
                                       (str_assembler_flags);
                              if DriverCom.verbose then
                                 TIO.Put_Line ("{ASSY} " &
                                                str_assembler_flags);
                              end if;
                              GNAT.OS_Lib.Spawn (
                                Program_Name => assy_fullpath,
                                Args         => args2 (args2'Range),
                                Success      => spawn_success);
                              GNAT.OS_Lib.Free (args2);
                           end;
                        end if;
                        if not DriverCom.save_temps and temp_created then
                           temp_created := False;
                           Ada.Directories.Delete_File (str_temp_file);
                        end if;
                     end if;
                  end;
               end loop;
            end if;
         end;
      end if;
   elsif not Did_Something then
      Commands.Display_Error (Commands.NoInputFiles);
   end if;

end Draco;

