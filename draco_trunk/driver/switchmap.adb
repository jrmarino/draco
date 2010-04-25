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


with Ada.Strings.Fixed;

package body SwitchMap is

   GroupedSwitches : TGroupedSwitches (Positive'First .. MaxListSize);
   FileList        : TFileList        (Positive'First .. MaxListSize);
   FileList_Count  : Natural := 0;
   Switch_Count    : Natural := 0;


   ----------------------------
   --  Analyze_Command_Line  --
   ----------------------------

   procedure Analyze_Command_Line is
      Wait4Next : Boolean := False;
      Holding   : SU.Unbounded_String := SU.Null_Unbounded_String;
      index     : Natural;
      stub      : String (1 .. 2);
      diff      : SU.Unbounded_String;
   begin
      for n in Positive'First .. MaxListSize loop
         if Wait4Next then
            --  Don't check value of argument, we are waiting to append this
            --  to the last switch.  Holding already has trailing space.

            Switch_Count := Switch_Count + 1;
            SU.Append (Holding, SU.To_Unbounded_String
                      (Ada.Command_Line.Argument (n)));
            GroupedSwitches (Switch_Count) := Holding;

            Holding   := SU.Null_Unbounded_String;
            Wait4Next := False;
         else

            if (Is_Switch (Ada.Command_Line.Argument (n))) then
               index := Map_Index (Ada.Command_Line.Argument (n));

               if index = 0 then
                  --  We didn't recognize the switch, so it's probably a
                  --  Draco/GNAT switch, which are all solo switches.

                  Switch_Count := Switch_Count + 1;
                  GroupedSwitches (Switch_Count) :=
                     SU.To_Unbounded_String (Ada.Command_Line.Argument (n));

               else

                  declare
                     SN : constant String := Ada.Strings.Fixed.Trim (
                                             OptionMap (index).Short_Name,
                                             Ada.Strings.Right);
                     LN : constant String := Ada.Strings.Fixed.Trim (
                                             OptionMap (index).Long_Name,
                                             Ada.Strings.Right);
                  begin
                     case OptionMap (index).Shadow is
                        when none =>
                           Switch_Count := Switch_Count + 1;
                           GroupedSwitches (Switch_Count) :=
                                            SU.To_Unbounded_String (SN);
                        when glued =>
                           stub := Ada.Command_Line.Argument (n) (1 .. 2);
                           if (stub = "--") then
                              diff := SU.To_Unbounded_String (
                                 Ada.Command_Line.Argument (n) (LN'Last + 1 ..
                                 Ada.Command_Line.Argument (n)'Last));

                           else
                              diff := SU.To_Unbounded_String (
                                 Ada.Command_Line.Argument (n) (SN'Last + 1 ..
                                 Ada.Command_Line.Argument (n)'Last));
                           end if;

                           Switch_Count := Switch_Count + 1;
                           GroupedSwitches (Switch_Count) :=
                                            SU.To_Unbounded_String (SN);
                           SU.Append (GroupedSwitches (Switch_Count), diff);

                        when trailing =>
                           Wait4Next := True;
                           Holding   := SU.To_Unbounded_String (SN);
                           SU.Append (Holding, " ");
                     end case;
                  end;

               end if;
            else
               FileList_Count := FileList_Count + 1;
               FileList (FileList_Count) :=
                     SU.To_Unbounded_String (Ada.Command_Line.Argument (n));
            end if;
         end if;

      end loop;


      null;
   end Analyze_Command_Line;



   -----------------------
   --  Get_Switch_List  --
   -----------------------

   function Get_Switch_List return TGroupedSwitches is
      result : TGroupedSwitches (Positive'First .. Switch_Count);
   begin
      for n in Positive'First .. Switch_Count loop
         result (n) := GroupedSwitches (n);
      end loop;
      return result;
   end Get_Switch_List;



   ---------------------
   --  Get_File_List  --
   ---------------------

   function Get_File_List return TFileList is
      result : TFileList (Positive'First .. FileList_Count);
   begin
      for n in Positive'First .. FileList_Count loop
         result (n) := FileList (n);
      end loop;
      return result;
   end Get_File_List;



   -------------------------
   -- Is_Switch [Private] --
   -------------------------

   function Is_Switch (Switch_Chars : in String) return Boolean is
   begin
      return Switch_Chars'Length > 1
        and then Switch_Chars (Switch_Chars'First) = '-';
   end Is_Switch;



   -------------------------
   -- Map_Index [Private] --
   -------------------------

   function Map_Index (Switch_Chars : in String) return Natural is
      result : Natural := 0;
      found  : Boolean := False;
      n      : Positive := Positive'First;
      sclen  : Natural;
      SCS    : TShortName;
      SCL    : TLongName;
   begin
      sclen  := Switch_Chars'Last;
      Ada.Strings.Fixed.Move (Switch_Chars, SCS, Drop => Ada.Strings.Right);
      Ada.Strings.Fixed.Move (Switch_Chars, SCL, Drop => Ada.Strings.Right);

      loop
         case OptionMap (n).Shadow is
            when none | trailing =>
               if (SCS = OptionMap (n).Short_Name) or
                  (SCL = OptionMap (n).Long_Name) then
                     found  := True;
                     result := n;
               end if;
            when glued =>
               declare
                  stub : constant String := Ada.Strings.Fixed.Trim (
                            OptionMap (n).Short_Name, Ada.Strings.Right);
               begin
                  if stub'Last <= sclen and then
                     stub = Switch_Chars (stub'First .. stub'Last) then
                     found  := True;
                     result := n;
                  end if;
               end;

               if not found then
                  declare
                     stub : constant String := Ada.Strings.Fixed.Trim (
                                OptionMap (n).Long_Name, Ada.Strings.Right);
                  begin
                     if stub'Last <= sclen and then
                        stub = Switch_Chars (stub'First .. stub'Last) then
                        found  := True;
                        result := n;
                     end if;
                  end;
               end if;

         end case;
         exit when found or n = OptionMap'Last;
         n := n + 1;
      end loop;

      return result;
   end Map_Index;

end SwitchMap;
