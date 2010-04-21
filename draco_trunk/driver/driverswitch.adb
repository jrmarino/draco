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

package body DriverSwitch is

   NumSet      : TSwitchIndex;
   SwitchList  : TSwitchList;
   SawPG       : Boolean := False;
   SawFramePtr : Boolean := False;
   GoNoGo      : Boolean := False;



   ---------------------------
   -- Post_Process [Public] --
   ---------------------------

   procedure Post_Process is
      c_exists     : Boolean;
      S_exists     : Boolean;
      Q_exists     : Boolean;
      gnatc_exists : Boolean;
      gnats_exists : Boolean;
      BitBucket    : SU.Unbounded_String;
   begin
      GoNoGo := True;

      c_exists     := Switch_Already_Set (SU.To_Unbounded_String ("-c"),
                                          Partial => False);
      S_exists     := Switch_Already_Set (SU.To_Unbounded_String ("-S"),
                                          Partial => False);
      Q_exists     := Switch_Already_Set (SU.To_Unbounded_String ("-S"),
                                          Partial => False);
      gnatc_exists := Switch_Already_Set (SU.To_Unbounded_String ("-gnatc"),
                                          Partial => True);
      gnats_exists := Switch_Already_Set (SU.To_Unbounded_String ("-gnats"),
                                          Partial => True);


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

      if gnatc_exists or gnats_exists then
         BitBucket := SU.To_Unbounded_String (DracoSystem.Host_Bit_Bucket
                        (DracoSystem.Native_System.Null_File_Type));
         Store_Switch (SU.To_Unbounded_String ("-o ") & BitBucket);
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



   --------------------------
   -- Dump_Flags [Private] --
   --------------------------

   function Dump_Flags (
      Switch  : in SU.Unbounded_String;
      Partial : in Boolean
    ) return SU.Unbounded_String
   is
      index   : TSwitchRange := TSwitchRange'First;
      foundit : Boolean := False;
      advance : Boolean;
      counter : Natural := 0;
      result  : SU.Unbounded_String := SU.Null_Unbounded_String;
   begin
      advance := NumSet > index;

      while advance loop
         case Partial is
            when False =>
               if Switch = SwitchList (index) then
                  foundit := True;
                  SU.Append (result, SwitchList (index));
               end if;
            when True =>
               if (Switch'Size <= SwitchList (index)'Size) and
                  (Switch = SU.Unbounded_Slice
                     (SwitchList (index), 1, Switch'Size)) then
                  if counter > 0 then
                     SU.Append (result, " ");
                  end if;
                  SU.Append (result, SwitchList (index));
                  counter := counter + 1;
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
      Partial : in Boolean
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
      if Switch_Chars'Last < 8 then
         return False;
      end if;

      return Switch_Chars (Switch_Chars'First .. Switch_Chars'First + 6) =
            "--param";
   end Parameter_Switch;


   --  function Host_Bit_bucket (

end DriverSwitch;

