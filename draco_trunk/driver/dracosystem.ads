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


package DracoSystem is

   type BitBucket_Variations is (POSIX, MINGW);
   subtype TArch is String (1 .. 16);
   subtype TOSName is String (1 .. 25);

   type RecSystem is record
      Null_File_Type : BitBucket_Variations;
      Architecture   : TArch;
      OS_Name        : TOSName;
      OS_Version     : Positive;
      Have_GNU_AS    : Boolean;
      Dash_For_Pipe  : Boolean;
      --  Draco Version
      --  GNAT Version
      --  Libexec Search Dir
      --  Library Search Dir?
   end record;



   --  This file is really a template file meant to be modified by the
   --  configuration script.  The Native_System constant record is built by
   --  That script using information specific to the host system.

   Native_System : constant RecSystem := (
      Null_File_Type => POSIX,
      Architecture   => "i386",
      OS_Name        => "DragonFly",
      OS_Version     => 200701,
      Have_GNU_AS    => True,
      Dash_For_Pipe  => True
   );


   function Host_Bit_Bucket (Variation : in BitBucket_Variations)
   return String;
   --  This function returns the name of the host bit bucket, e.g. /dev/null
   --  on posix systems.

end DracoSystem;
