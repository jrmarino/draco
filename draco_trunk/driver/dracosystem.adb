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


with Ada.Strings.Fixed;

package body DracoSystem is


   -----------------------
   --  Host_Bit_Bucket  --
   -----------------------

   function Host_Bit_Bucket (Variation : in BitBucket_Variations)
   return String is
   begin
      case Variation is
         when POSIX => return "/dev/null";
         when MINGW => return "nul";
      end case;
   end Host_Bit_Bucket;


   ----------------
   --  Set_Arch  --
   ----------------

   function Set_Arch (value : String) return TArch is
      result : TArch := (others => ' ');
   begin
      Ada.Strings.Fixed.Move (value, result, Drop => Ada.Strings.Right);
      return result;
   end Set_Arch;


   ------------------
   --  Set_OSName  --
   ------------------

   function Set_OSName (value : String) return TOSName is
      result : TOSName := (others => ' ');
   begin
      Ada.Strings.Fixed.Move (value, result, Drop => Ada.Strings.Right);
      return result;
   end Set_OSName;


   ------------------
   --  Set_OSName  --
   ------------------

   function Set_Target (value : String) return TTarget is
      result : TTarget := (others => ' ');
   begin
      Ada.Strings.Fixed.Move (value, result, Drop => Ada.Strings.Right);
      return result;
   end Set_Target;


   -------------------
   --  Set_Version  --
   -------------------

   function Set_Version (value : String) return TVersion is
      result : TVersion := (others => ' ');
   begin
      Ada.Strings.Fixed.Move (value, result, Drop => Ada.Strings.Right);
      return result;
   end Set_Version;


   -------------------
   --  Set_DefPath  --
   -------------------

   function Set_DefPath (value : String) return TDefPath is
      result : TDefPath := (others => ' ');
   begin
      Ada.Strings.Fixed.Move (
         Source => value,
         Target => result,
         Drop   => Ada.Strings.Right,
         Pad    => ':'
      );
      return result;
   end Set_DefPath;

end DracoSystem;

