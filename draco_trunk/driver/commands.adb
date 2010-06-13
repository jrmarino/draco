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


with DracoSystemSpecs;
with Ada.Strings.Fixed;

package body Commands is

   Path_Lib        : Commands.TBinPath;
   Path_Libexec    : Commands.TBinPath;


   -------------------------
   --  Print_Search_Dirs  --
   -------------------------

   procedure Print_Search_Dirs is
      nd_lib:     Natural;
      nd_libexec: Natural;
   begin
      nd_lib     := Number_Of_Directories (Path_Lib);
      nd_libexec := Number_Of_Directories (Path_Libexec);
      TIO.Put_Line ("Programs:");
      if nd_lib > 0 then
         for x in Positive range 1 .. nd_lib loop
            TIO.Put_Line ("  " & Directory (Path_Lib, x));
         end loop;
      end if;
      TIO.Put_Line ("");
      TIO.Put_Line ("Libraries:");
      if nd_libexec > 0 then
         for x in Positive range 1 .. nd_libexec loop
            TIO.Put_Line ("  " & Directory (Path_Libexec, x));
         end loop;
      end if;
   end Print_Search_Dirs;


   ---------------------------
   --  Directory [private]  --
   ---------------------------

   function Directory (path: TBinPath; index: Positive) return String is

      head    : TPathLen := 1;
      tail    : TPathLen := 1;
      arrow   : TPathLen := 1;
      current : Natural  := 0;
      waiting : Boolean := true;
      aborted : Boolean;
   begin
      counter:
         loop
            aborted := waiting and (path (arrow) = ':');
            exit counter when aborted;

            if (path (arrow) = ':') then
               waiting := true;
            else
               if waiting then
                  waiting := false;
                  current := current + 1;
                  if current = index then
                     head    := arrow;
                  end if;
               end if;
               if current = index then
                  tail := arrow;
               end if;
            end if;
            exit counter when current > index;
            exit counter when arrow = path'Length;
            arrow := arrow + 1;
         end loop counter;
      if current >= index then
         return path (head .. tail);
      else
         return "";
      end if;
   end;


   ---------------------------------------
   --  Number_Of_Directories [private]  --
   ---------------------------------------

   function Number_Of_Directories (path: TBinPath) return Natural is
      result  : Natural  := 0;
      arrow   : TPathLen := 1;
      waiting : Boolean := true;
      aborted : Boolean;
   begin
      counter:
         loop
            aborted := waiting and (path (arrow) = ':');
            exit counter when aborted;

            if (path (arrow) = ':') then
               waiting := true;
            else
               if waiting then
                  waiting := false;
                  result  := result + 1;
               end if;
            end if;
            exit counter when arrow = path'Length;
            arrow := arrow + 1;
         end loop counter;
      return result;
   end;


   ------------------------
   --  Initialize_Paths  --
   ------------------------

   procedure Initialize_Paths is
   begin
      Ada.Strings.Fixed.Move (
         source => DracoSystemSpecs.Native_System.Path_lib,
         target => Path_Lib,
         Drop   => Ada.Strings.Right,
         pad    => ':'
      );
      Ada.Strings.Fixed.Move (
         source => DracoSystemSpecs.Native_System.Path_libexec,
         target => Path_Libexec,
         Drop   => Ada.Strings.Right,
         pad    => ':'
      );
   end Initialize_Paths;


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
