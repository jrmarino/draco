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


with Ada.Strings.Unbounded;
with Ada.Text_IO;

package DriverSwitch is

   package SU  renames Ada.Strings.Unbounded;
   package TIO renames Ada.Text_IO;

   switch_err_1 : constant String :=
                  "-pg and -fomit-frame-pointer are incompatible";
   switch_err_2 : constant String :=
                  "The -c (compile) or -S (assembly) switch is required.";

   type TSwitchIndex is range 0 .. 100;
   subtype TSwitchRange is TSwitchIndex
           range TSwitchIndex'First .. TSwitchIndex'Last - 1;

   type TSwitchList is array (TSwitchRange) of SU.Unbounded_String;

   function Is_Switch (Switch_Chars : in String) return Boolean;
   --  Returns True if Switch_Chars is at least two characters long, and the
   --  first character is an hyphen ('-').

   procedure Set_Switch (Switch_Chars : in String);
   --  The switch will first be validated.  If the string is indeed a switch
   --  and it meets the criteria of being sent to the ada compiler (meaning
   --  it's a dedicated Ada switch, a special case, or starts with -O, -W,
   --  -w, -f, -d, -g, or -m) then it will be internally stored, otherwise
   --  it's simply ignored.  Some switches are transformed per GCC Ada Specs

   procedure Post_Process;
   --  Checks will be done for incompatible or missing switches.
   --  Additionally, new switches are added based on what was provided
   --  previously.  It will also set the go/no go flag.

private

   procedure Store_Switch (Switch : in SU.Unbounded_String);
   --  When presented a new switch string, a search will be performed to make
   --  sure it doesn't already exist.  If there is space in the preallocated
   --  array, the switch will be saved, otherwise it will be ignored.

   function Switch_Already_Set (
      Switch : in SU.Unbounded_String
   ) return Boolean;
   --  Systematically search the previously set switches and return
   --  true if an entry matches the query.

   function Dedicated_Compiler_Switch (
      Switch_Chars : in String
   ) return Boolean;
   --  Returns True if first five characters of the 6+ character input
   --  string match "-gnat", "-gant", or "-drac"

   function Parameter_Switch (Switch_Chars : in String) return Boolean;
   --  Returns True if first seven characters of the 8+ character input
   --  string match "--param"

end DriverSwitch;

