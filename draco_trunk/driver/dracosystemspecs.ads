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


with DracoSystem;

package DracoSystemSpecs is

   --  This file is really a template file meant to be modified by the
   --  configuration script.  The Native_System constant record is built by
   --  That script using information specific to the host system.

   Native_System : constant DracoSystem.RecSystem := (
      Null_File_Type => DracoSystem.POSIX,
      Backend        => DracoSystem.X86,
      Draco_Version  => DracoSystem.Set_Version ("4.6.20100424"),
      MachineTarget  => DracoSystem.Set_Target  ("i386-backplane-dragonfly2.6"),
      Architecture   => DracoSystem.Set_Arch    ("i386"),
      OS_Name        => DracoSystem.Set_OSName  ("DragonFly"),
      OS_Version     => 200602,
      Have_GNU_AS    => True,
      Dash_For_Pipe  => False,
      CPU_AutoDetect => True,
      CC_Flags       => DracoSystem.i386
   );

end DracoSystemSpecs;
