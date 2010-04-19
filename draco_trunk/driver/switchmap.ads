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


with Ada.Command_Line;
with Ada.Strings.Unbounded;

package SwitchMap is

   package SU  renames Ada.Strings.Unbounded;

   type    TShadow    is (none, glued, trailing);
   subtype TLongName  is String (1 .. 35);
   subtype TShortName is String (1 .. 30);

   type RecSwitch is record
      Long_Name  : TLongName;
      Short_Name : TShortName;
      Shadow     : TShadow;
   end record;

   type TOptionMap       is array (Positive range <>) of RecSwitch;
   type TGroupedSwitches is array (Positive range <>) of SU.Unbounded_String;
   type TFileList        is array (Positive range <>) of SU.Unbounded_String;

   MaxListSize : constant Natural := Ada.Command_Line.Argument_Count;

   OptionMap : constant TOptionMap := (
      ("--all-warnings", "-Wall", none),
      ("--ansi", "-ansi", none),
      ("--assemble", "-S", none),
      ("--assert", "-A", trailing),
      ("--classpath", "-fclasspath=", glued),
      ("--bootclasspath", "-fbootclasspath=", glued),
      ("--CLASSPATH", "-fclasspath=", glued),
      ("--combine", "-combine", none),
      ("--comments", "-C", none),
      ("--comments-in-macros", "-CC", none),
      ("--compile", "-c", none),
      ("--debug", "-g", glued),
      ("--define-macro", "-D", glued),
      ("--dependencies", "-M", none),
      ("--dump", "-d", trailing),
      ("--dumpbase", "-dumpbase", trailing),
      ("--dumpdir", "-dumpdir", trailing),
      ("--encoding", "-fencoding=", glued),
      ("--entry", "-e", none),
      ("--extra-warnings", "-W", none),
      ("--extdirs", "-fextdirs=", glued),
      ("--for-assembler", "-Wa", trailing),
      ("--for-linker", "-Xlinker", trailing),
      ("--force-link", "-u", trailing),
      ("--coverage", "-coverage", none),
      ("--imacros", "-imacros", trailing),
      ("--include", "-include", trailing),
      ("--include-barrier", "-I-", none),
      ("--include-directory", "-I", glued),
      ("--include-directory-after", "-idirafter", trailing),
      ("--include-prefix", "-iprefix", trailing),
      ("--include-with-prefix", "-iwithprefix", trailing),
      ("--include-with-prefix-before", "-iwithprefixbefore", trailing),
      ("--include-with-prefix-after", "-iwithprefix", trailing),
      ("--language", "-x", trailing),
      ("--library-directory", "-L", trailing),
      ("--machine", "-m", glued),
      ("--machine-", "-m", glued),
      ("--no-canonical-prefixes", "-no-canonical-prefixes", none),
      ("--no-integrated-cpp", "-no-integrated-cpp", none),
      ("--no-line-commands", "-P", none),
      ("--no-precompiled-includes", "-noprecomp", none),
      ("--no-standard-includes", "-nostdinc", none),
      ("--no-standard-libraries", "-nostdlib", none),
      ("--no-warnings", "-w", none),
      ("--optimize", "-O", glued),
      ("--output", "-o", trailing),
      ("--output-class-directory", "-foutput-class-dir=", glued),
      ("--param", "--param", trailing),
      ("--pass-exit-codes", "-pass-exit-codes", none),
      ("--pedantic", "-pedantic", none),
      ("--pedantic-errors", "-pedantic-errors", none),
      ("--pie", "-pie", none),
      ("--pipe", "-pipe", none),
      ("--prefix", "-B", trailing),
      ("--preprocess", "-E", none),
      ("--print-search-dirs", "-print-search-dirs", none),
      ("--print-file-name", "-print-file-name=", glued),
      ("--print-libgcc-file-name", "-print-libgcc-file-name", none),
      ("--print-missing-file-dependencies", "-MG", none),
      ("--print-multi-lib", "-print-multi-lib", none),
      ("--print-multi-directory", "-print-multi-directory", none),
      ("--print-multi-os-directory", "-print-multi-os-directory", none),
      ("--print-prog-name", "-print-prog-name=", glued),
      ("--print-sysroot", "-print-sysroot", none),
      ("--print-sysroot-headers-suffix", "-print-sysroot-headers-suffix",
          none),
      ("--profile", "-p", none),
      ("--profile-blocks", "-a", none),
      ("--quiet", "-q", none),
      ("--resource", "-fcompile-resource=", glued),
      ("--save-temps", "-save-temps", none),
      ("--shared", "-shared", none),
      ("--silent", "-q", none),
      ("--specs", "-specs=", glued),
      ("--static", "-static", none),
      ("--std", "-std=", glued),
      ("--symbolic", "-symbolic", none),
      ("--sysroot", "--sysroot=", glued),
      ("--time", "-time", none),
      ("--trace-includes", "-H", none),
      ("--traditional", "-traditional", none),
      ("--traditional-cpp", "-traditional-cpp", none),
      ("--trigraphs", "-trigraphs", none),
      ("--undefine-macro", "-U", glued),
      ("--user-dependencies", "-MM", none),
      ("--verbose", "-v", none),
      ("--warn-", "-W", glued),
      ("--write-dependencies", "-MD", none),
      ("--write-user-dependencies", "-MMD", none),
      ("--", "-f", glued)
   );

   procedure Analyze_Command_Line;
   --  This procedure needs to be run first, and only once.  It runs through
   --  the command line arguments and splits them out between files and
   --  switches and groups the switches as necessary.  These are internally
   --  stored to avoid the inefficiency of repeating this exercise.

   function  Get_Switch_List return TGroupedSwitches;
   --  This function returns an array of switches that were provided, and these
   --  are grouped as necessary, e.g. "-o foo.s" would have been represented by
   --  two command line arguments, but it is considered a single switch.  The
   --  expectations of each switch are defined by the TShadow value of the
   --  option map above.

   function  Get_File_List return TFileList;
   --  This function returns an array of file names to be compiled by Draco.
   --  Each file will be sent a subset of the switch list that is determined at
   --  the same time as the file list.  The Ada "specs" file will determine
   --  what subset of switches and well as order and substitions, but this is
   --  done in another unit.

private

   function Is_Switch (Switch_Chars : in String) return Boolean;
   --  Returns True if Switch_Chars is at least two characters long, and the
   --  first character is an hyphen ('-').

   function Map_Index (Switch_Chars : in String) return Natural;
   --  This function searches the constant option map for a match with a
   --  switch, Either the long form or the short form.  The Shadow value will
   --  determine if it will look for a partial match or a full one.  If no
   --  match is found, then a zero is returned;

end SwitchMap;
