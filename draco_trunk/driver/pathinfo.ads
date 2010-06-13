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


with GNAT.OS_Lib;
with Ada.Strings.Unbounded;

package PathInfo is

   package SU renames Ada.Strings.Unbounded;

   SDot  : constant String := ".";
   USDot : constant SU.Unbounded_String := SU.To_Unbounded_String (SDot);
   Slash : constant String := String'(1 => GNAT.OS_Lib.Directory_Separator);


   type RecPathInfo is record
      dirname   : SU.Unbounded_String;
      basename  : SU.Unbounded_String;
      filename  : SU.Unbounded_String;
      extension : SU.Unbounded_String;
   end record;


   function Dirname   (path : in SU.Unbounded_String) return
      SU.Unbounded_String;
   --  This function will accept a file or directory path as its sole argument
   --  and it will return the directory portion of the path.  If the path
   --  given consists solely of a filename (e.g. no directory separators)
   --  then the function will return ".".  The result will not include
   --  trailing separators, except in the case of the root folder.


   function Basename  (path : in SU.Unbounded_String) return
      SU.Unbounded_String;
   --  This function will accept a path to a file, and return the
   --  filename component of the path.  The result will trim the last period
   --  and any following characters if it exists.  This is normally defined as
   --  a file extension.  If this behavior is undesired, use the "filename"
   --  function instead.


   function Extension (path : in SU.Unbounded_String) return
      SU.Unbounded_String;
   --  This function will accept a path to a file, and return just the
   --  file extension, if it exists.  A file extension is defined as the final
   --  period within a filename, and all characters that follow it.  If no
   --  period exists, a null string will be returned.


   function Filename  (path : in SU.Unbounded_String) return
      SU.Unbounded_String;
   --  The function will accept a path to a file.  Internally it will strip
   --  out the path to the directory, if one was provided, and it will return
   --  just the filename without any directory separators.


   function Info  (path : in SU.Unbounded_String) return RecPathInfo;
   --  This function will accept a path to a file or directory, and it will
   --  split this information up into various components: path to directory,
   --  full filename, base filename, and filename extension.


end PathInfo;
