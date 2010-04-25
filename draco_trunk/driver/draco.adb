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


with SwitchMap;
with DriverSwitch;
with Ada.Strings.Unbounded;

procedure Draco is

   package SU renames Ada.Strings.Unbounded;

   DriverCom       : DriverSwitch.RecDriverCommands;
begin
   SwitchMap.Analyze_Command_Line;

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

   DriverSwitch.Post_Process;

end Draco;

