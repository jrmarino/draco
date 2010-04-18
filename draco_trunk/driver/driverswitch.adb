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


with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body DriverSwitch is

   NumSet      : TSwitchIndex;
   SwitchList  : TSwitchList;
   --  SawCorS     : Boolean := False;
   SawPG       : Boolean := False;
   SawFramePtr : Boolean := False;
   GoNoGo      : Boolean := False;
   --  SawQ        : Boolean := False;



   ---------------------------
   -- Post_Process [Public] --
   ---------------------------

   procedure Post_Process is
      S_exists : Boolean;
      c_exists : Boolean;
   begin
      GoNoGo := True;
      if SawPG and SawFramePtr then
         GoNoGo := False;
         TIO.Put_Line (switch_err_1);
      end if;

      c_exists := Switch_Already_Set (SU.To_Unbounded_String ("-c"));
      S_exists := Switch_Already_Set (SU.To_Unbounded_String ("-S"));
      if not c_exists and not S_exists then
         GoNoGo := False;
         TIO.Put_Line (switch_err_2);         
      end if;

      if not Switch_Already_Set (SU.To_Unbounded_String ("-Q")) then
         Store_Switch (SU.To_Unbounded_String ("-quiet"));
      end if;

      
   end Post_Process;


   ------------------------
   -- Is_Switch [Public] --
   ------------------------

   function Is_Switch (Switch_Chars : in String) return Boolean is
   begin
      return Switch_Chars'Length > 1
        and then Switch_Chars (Switch_Chars'First) = '-';
   end Is_Switch;



   -------------------------
   -- Set_Switch [Public] --
   -------------------------

   procedure Set_Switch (Switch_Chars : in String) is
      DCS     : SU.Unbounded_String;
      KeyChar : Character;
   begin
      if not Is_Switch (Switch_Chars) then
         return;
      end if;

      if Dedicated_Compiler_Switch (Switch_Chars) then
         DCS := SU.To_Unbounded_String ("-gnat") &
               Switch_Chars (Switch_Chars'First + 5 .. Switch_Chars'Last);
         Store_Switch (DCS);

         if DCS = "-gnatea" then
            Store_Switch (SU.To_Unbounded_String ("-gnatez"));
         end if;
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

      --  if Switch_Chars = "-Q" then
      --     SawQ := True;
      --  end if;

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


   ----------------------------
   -- Store_Switch [Private] --
   ----------------------------

   procedure Store_Switch (Switch : in SU.Unbounded_String) is
   begin
      if (NumSet = TSwitchIndex'Last) or
         Switch_Already_Set (Switch) then return;
      end if;

      SwitchList (NumSet) := Switch;
      NumSet := NumSet + 1;
   end Store_Switch;



   ----------------------------------
   -- Switch_Already_Set [Private] --
   ----------------------------------

   function Switch_Already_Set (
      Switch : in SU.Unbounded_String
    ) return Boolean
   is
      index   : TSwitchRange := TSwitchRange'First;
      result  : Boolean := False;
      advance : Boolean;
   begin
      advance := NumSet > index;

      while advance loop
         if SwitchList (index) = Switch then
            result  := True;
            exit;
         end if;

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
      if Switch_Chars'Last < 8 then
         return False;
      end if;

      return Switch_Chars (Switch_Chars'First .. Switch_Chars'First + 7) =
            "--param";
   end Parameter_Switch;

end DriverSwitch;

