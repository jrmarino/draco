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


with DracoSystemSpecs;

package body Commands is

   ---------------------
   --  Display_Error  --
   ---------------------

   procedure Display_Error (Error : TError) is
      prefix : constant String := "DRACO error: ";
   begin
      case Error is
         when NoInputFiles => TIO.Put_Line (prefix & msg0);
      end case;
   end Display_Error;


   --------------------
   --  Dump_Machine  --
   --------------------

   procedure Dump_Machine is
   begin
      TIO.Put_Line (DracoSystemSpecs.Native_System.MachineTarget);
   end Dump_Machine;


   --------------------
   --  Dump_Version  --
   --------------------

   procedure Dump_Version is
   begin
      TIO.Put_Line (DracoSystemSpecs.Native_System.Draco_Version);
   end Dump_Version;

end Commands;
