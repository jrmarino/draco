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


with Ada.Text_IO;

package Commands is

   package TIO renames Ada.Text_IO;

   type TError is (NoInputFiles);
   subtype TBinPath is String (1 .. 1000);
   subtype TPathLen is Positive range 1 .. TBinPath'Length;
   msg0 : constant String := "No input file paths were provided.";

   procedure Display_Error (Error : TError);
   --  Given an error code, this procedure displays the appropriate message.

   procedure Dump_Machine;
   --  This displays the value of MachineTarget field in the system spec.

   procedure Dump_Version;
   --  This displays the value of Draco_Version field in the system spec.

   procedure Initialize_Paths;
   --  This sets the path variables to the value of the Draco System.

   procedure Print_Search_Dirs;
   --  This displays the current paths for the library and libexec directories

private

   function Number_Of_Directories (path: TBinPath) return Natural;
   --  This helper function looks for directory separator characters to
   --  determine how many directories are currently in the search path.
   --  If separator (:) appears consecutively, then the search will stop.
   --  The dracosystem constant must be correctly formatted as no validation
   --  is done here.

   function Directory (path: TBinPath; index: Positive) return String;
   --  This helper function will return a substring of the path given the
   --  index (1,2,3...)


end Commands;
