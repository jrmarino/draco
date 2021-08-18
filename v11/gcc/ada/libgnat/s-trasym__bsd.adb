------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           S Y S T E M . T R A C E B A C K . S Y M B O L I C              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1999-2018, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Run-time symbolic traceback support
--  This file is based on the work by Juergen Pfiefer which is still used
--  today to provide symbolic traceback support for gnu/kFreeBSD.
--  Incorporated in GNAT-AUX by John Marino <http://www.dragonlace.net>

with Ada.Exceptions.Traceback; use Ada.Exceptions.Traceback;
with System.Soft_Links;

package body System.Traceback.Symbolic is

   --  Note that Suppress_Hex is ignored in this version of this package.

   package TSL renames System.Soft_Links;

   --  To perform the raw addresses to symbolic form translation we rely on a
   --  libaddr2line symbolizer which examines debug info from a provided
   --  executable file name, and an absolute path is needed to ensure the file
   --  is always found. This is "__gnat_locate_exec_on_path (gnat_argv [0])"
   --  for our executable file, a fairly heavy operation so we cache the
   --  result.

   Exename : System.Address;
   --  Pointer to the name of the executable file to be used on all
   --  invocations of the libaddr2line symbolization service.

   Exename_Resolved : Boolean := False;
   --  Flag to indicate whether we have performed the executable file name
   --  resolution already. Relying on a not null Exename for this purpose
   --  would be potentially inefficient as this is what we will get if the
   --  resolution attempt fails.

   ------------------------
   -- Symbolic_Traceback --
   ------------------------

   function Symbolic_Traceback
     (Traceback : System.Traceback_Entries.Tracebacks_Array) return String
   is
      procedure convert_addresses
        (filename : System.Address;
         addrs    : System.Address;
         n_addrs  : Integer;
         buf      : System.Address;
         len      : System.Address);
      pragma Import (C, convert_addresses, "convert_addresses");
      --  This is the procedure version of the Ada-aware addr2line. It places
      --  in BUF a string representing the symbolic translation of the N_ADDRS
      --  raw addresses provided in ADDRS, looked up in debug information from
      --  FILENAME. LEN points to an integer which contains the size of the
      --  BUF buffer at input and the result length at output.
      --
      --  Note that this procedure is *not* thread-safe.

      type Argv_Array is array (0 .. 0) of System.Address;
      gnat_argv : access Argv_Array;
      pragma Import (C, gnat_argv, "gnat_argv");

      function locate_exec_on_path
        (c_exename : System.Address) return System.Address;
      pragma Import (C, locate_exec_on_path, "__gnat_locate_exec_on_path");

      B_Size : constant Integer := 256 * Traceback'Length;
      Len    : Integer := B_Size;
      Res    : String (1 .. B_Size);

   begin
      if Traceback'Length = 0 then
         return "";
      end if;

      --  If our input set of raw addresses is not empty, resort to the
      --  libaddr2line service to symbolize it all.

      --  Compute, cache and provide the absolute path to our executable file
      --  name as the binary file where the relevant debug information is to be
      --  found. If the executable file name resolution fails, we have no
      --  sensible basis to invoke the symbolizer at all.

      --  Protect all this against concurrent accesses explicitly, as the
      --  underlying services are potentially thread unsafe.

      TSL.Lock_Task.all;

      if not Exename_Resolved then
         Exename := locate_exec_on_path (gnat_argv (0));
         Exename_Resolved := True;
      end if;

      if Exename /= System.Null_Address then
         Len := Res'Length;
         convert_addresses
           (Exename, Traceback'Address, Traceback'Length,
            Res (1)'Address, Len'Address);
      end if;

      TSL.Unlock_Task.all;

      --  Return what the addr2line symbolizer has produced if we have called
      --  it (the executable name resolution succeeded), or an empty string
      --  otherwise.

      if Exename /= System.Null_Address then
         return Res (1 .. Len);
      else
         return "";
      end if;
   end Symbolic_Traceback;

   --  "No_Hex" is ignored in this version, because otherwise we have nothing
   --  at all to print.

   function Symbolic_Traceback_No_Hex
     (Traceback : System.Traceback_Entries.Tracebacks_Array) return String is
   begin
      return Symbolic_Traceback (Traceback);
   end Symbolic_Traceback_No_Hex;

   function Symbolic_Traceback
     (E : Ada.Exceptions.Exception_Occurrence) return String
   is
   begin
      return Symbolic_Traceback (Tracebacks (E));
   end Symbolic_Traceback;

   function Symbolic_Traceback_No_Hex
     (E : Ada.Exceptions.Exception_Occurrence) return String is
   begin
      return Symbolic_Traceback (E);
   end Symbolic_Traceback_No_Hex;

   ------------------
   -- Enable_Cache --
   ------------------

   procedure Enable_Cache (Include_Modules : Boolean := False) is
   begin
      null;
   end Enable_Cache;

end System.Traceback.Symbolic;
