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


with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
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
   type TDetectMethod is (ARCH, TUNE);

   type RecDriverCommands is record
      version     : Boolean := False;
      verbose     : Boolean := False;
      pipe        : Boolean := False;
      help        : Boolean := False;
      dumpmachine : Boolean := False;
      dumpversion : Boolean := False;
      save_temps  : Boolean := False;
      psearchdir  : Boolean := False;
   end record;

   function Commands return RecDriverCommands;
   --  During the switch scanning process, if driver commands are found
   --  they are recorded internally inside the RecDriverCommands record and
   --  this function provides that information publically.

   procedure Build_Arguments (
       source_file     : in  SU.Unbounded_String;
       compiler_flags  : out SU.Unbounded_String;
       assembler_flags : out SU.Unbounded_String
   );
   --  This key procedure builds the switches for the compiler and also for
   --  the assembler.  They are separate because this driver spawns both
   --  programs separately, unless the -S, -gnatc, or -gnats switches are
   --  provided by the user.


   function Proceed return Boolean;
   --  After building the arguments, verify that there were no fatch errors
   --  such as missing both -c and -S flags.  If the switches provided are
   --  valid, then this function will return True.


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

   function Detect_Local_CPU (DetectMethod : in TDetectMethod)
   return SU.Unbounded_String;
   --  This is an interface to a complex C routine of the GCC i386 driver.  It
   --  automatically selects cpu-specific switches if it can.  This function
   --  can be deleted after GiGi is replaced by DLC.

   procedure Add_CC_Flags_Per_Target_Spec (
      collection : in out SU.Unbounded_String
   );
   --  This procedure passes processor and subtarget specific flags back to
   --  the GCC backend.  This is temporary.  When we replace GiGi, the driver
   --  with be modified to remove the assembler execution (and replace it with
   --  LLVM), and this is one of the procedures that will fall out.

   procedure TackOn (
      collection : in out SU.Unbounded_String;
      flag       : in SU.Unbounded_String
   );
   procedure TackOn (
      collection : in out SU.Unbounded_String;
      flag       : in String
   );
   --  This is a helper function.  It basically appends a string to another
   --  string, but if the provided counter is greater than zero, it will
   --  join them with a space, and then increment the counter.



   function Dump_Flags (
      Switch  : in String;
      Partial : in Boolean
   ) return SU.Unbounded_String;
   --  This function helps construct the argument string.  It can locate a
   --  specific switch or a slew of switches using a wildcard (e.g. I*).
   --  The wildcard is indicated by setting Partial to True.  All switches
   --  are separated by a single space.

   procedure Store_Switch (SSwitch : in String);
   --  When presented a new switch string, a search will be performed to make
   --  sure it doesn't already exist.  If there is space in the preallocated
   --  array, the switch will be saved, otherwise it will be ignored.

   function Switch_Already_Set (
      SSwitch : in String;
      Partial : in Boolean := False
   ) return Boolean;
   --  Systematically search the previously set switches and return
   --  true if an entry matches the query.  If a partial match is sought
   --  through a wildcard (e.g. -gnatc*) then set Partial = true

   function Dedicated_Compiler_Switch (
      Switch_Chars : in String
   ) return Boolean;
   --  Returns True if first five characters of the 6+ character input
   --  string match "-gnat", "-gant", or "-drac"

   function Parameter_Switch (Switch_Chars : in String) return Boolean;
   --  Returns True if first seven characters of the 8+ character input
   --  string match "--param"

end DriverSwitch;

