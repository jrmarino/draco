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


package body PathInfo is


   --------------
   --  Dirname --
   --------------

   function Dirname (path : in SU.Unbounded_String) return
      SU.Unbounded_String is
      result : SU.Unbounded_String;
      index : Natural;
   begin
      index := SU.Index (
          Source  => path,
          Pattern => Slash,
          Going   => Ada.Strings.Backward
      );
      case index is
         when 0      => result := USDot;
         when 1      => result := SU.To_Unbounded_String (Slash);
         when others => result := SU.Unbounded_Slice (path, 1, index - 1);
      end case;

      return result;
   end Dirname;


   ---------------
   --  Basename --
   ---------------

   function Basename  (path : in SU.Unbounded_String) return
      SU.Unbounded_String is
      full_filename : SU.Unbounded_String;
      result : SU.Unbounded_String;
      index  : Natural;
   begin
      full_filename := Filename (path);
      index := SU.Index (
          Source  => full_filename,
          Pattern => SDot,
          Going   => Ada.Strings.Backward
      );
      if index <= 1 then
         result := full_filename;
      else
         result := SU.Unbounded_Slice (full_filename, 1, index - 1);
      end if;

      return result;
   end Basename;



   -----------------
   --  Extension  --
   -----------------

   function Extension (path   : in SU.Unbounded_String) return
      SU.Unbounded_String is
      full_filename : SU.Unbounded_String;
      result  : SU.Unbounded_String;
      namelen : Natural;
      index   : Natural;
   begin
      full_filename := Filename (path);
      namelen       := SU.Length (full_filename);
      index := SU.Index (
          Source  => full_filename,
          Pattern => SDot,
          Going   => Ada.Strings.Backward
      );
      if index <= 1 then
         result := SU.Null_Unbounded_String;
      elsif index = namelen then
         result := SU.Null_Unbounded_String;
      else
         result := SU.Unbounded_Slice
                   (full_filename, index + 1, namelen);
      end if;

      return result;
   end Extension;


   ----------------
   --  Filename  --
   ----------------

   function Filename  (path   : in SU.Unbounded_String) return
      SU.Unbounded_String is
      result : SU.Unbounded_String;
      index : Natural;
      pathlen : Natural;
   begin
      pathlen := SU.Length (path);
      index := SU.Index (
          Source  => path,
          Pattern => Slash,
          Going   => Ada.Strings.Backward
      );
      if index = 0 then
         result := path;
      elsif index = pathlen then
         result := SU.Null_Unbounded_String;
      else
         result := SU.Unbounded_Slice (path, index + 1,  pathlen);
      end if;

      return result;
   end Filename;


   function Info (path   : in SU.Unbounded_String) return RecPathInfo is
      result  : RecPathInfo;
      pathlen : Natural;
      namelen : Natural;
      index   : Natural;
   begin
      pathlen := SU.Length (path);
      index := SU.Index (
          Source  => path,
          Pattern => Slash,
          Going   => Ada.Strings.Backward
      );
      case index is
         when 0      => result.dirname := USDot;
         when 1      => result.dirname := SU.To_Unbounded_String (Slash);
         when others => result.dirname := SU.Unbounded_Slice
                                          (path, 1, index - 1);
      end case;

      if index = 0 then
         result.filename := path;
      elsif index = pathlen then
         result.filename := SU.Null_Unbounded_String;
      else
         result.filename := SU.Unbounded_Slice (path, index + 1,  pathlen);
      end if;

      index := SU.Index (
          Source  => result.filename,
          Pattern => SDot,
          Going   => Ada.Strings.Backward
      );
      namelen := SU.Length (result.filename);
      if (index <= 1) or (index = namelen) then
            result.extension := SU.Null_Unbounded_String;
      else
            result.extension := SU.Unbounded_Slice (result.filename,
                                index + 1, namelen);
      end if;

      if index <= 1 then
         result.basename := result.filename;
      else
         result.basename := SU.Unbounded_Slice (result.filename, 1, index - 1);
      end if;

      return result;
   end Info;

end PathInfo;
