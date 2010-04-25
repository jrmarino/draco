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
   type BackendTargets is (X86, ARM, SPARC, POWERPC, MIPS, IA64, ALPHA);
   subtype TArch    is String (1 .. 16);
   subtype TOSName  is String (1 .. 25);
   subtype TTarget  is String (1 .. 50);
   subtype TVersion is String (1 .. 14);
   type CC1_SPEC is (
      blank,     --  default (includes %{profile:-p})
      g_star,    --  %{G*} alpha/elf.h ia64/ia64.h ia64/linux
      android,   --  %{mandroid: %{!fexceptions:-fno-exceptions};:} arm/eabi.h
      symbian,   --  fbuiltin/fvis/fshort-enums/fshort-wchar arm/symbian.h
      vxworks,   --  12 lines! arm/vxworks.h
      i386,      --  i386 CC1_CPU_SPEC + local detect i386.h, gnu.h
      darwin,    --  i386 + 3 lines i386/darwin.h
      x86linux,  --  i386 + %{G*} i386/linux.h i386/x86-64.h
      stdmips,   --  4 lines, mips/mips.h mips/linux.h
      mipsiris,  --  std mips + %{static: -mno-abicalls} mips/iris.h
      stdppc,    --  2 lines! rs6000/aix.h
      lynxppc,   --  rs6000/lynx.h
      darwinppc, --  stdppc + 4 lines rs6000/darwin.h
      sysv4ppc,  --  stdppc + ~22 lines!!! rs6000/sysv4.h
      vxppc,     --  4 lines, rs6000/vxworks.h
      spclinux,  --  sparc/linux.h
      spclin64,  --  sparc/linux64.h
      spcnet,    --  sparc/netbsd-elf.h
      spcsol2,   --  sparc/sol2-bi.h
      sparc      --  sparc/sparc.h
   );

   type RecSystem is record
      Null_File_Type : BitBucket_Variations;
      Backend        : BackendTargets;
      Draco_Version  : TVersion;
      MachineTarget  : TTarget;
      Architecture   : TArch;
      OS_Name        : TOSName;
      OS_Version     : Positive;
      Have_GNU_AS    : Boolean;
      Dash_For_Pipe  : Boolean;
      CPU_AutoDetect : Boolean;
      CC_Flags       : CC1_SPEC;
      --  Draco Version
      --  GNAT Version
      --  Libexec Search Dir
      --  Library Search Dir?
   end record;

   function Host_Bit_Bucket (Variation : in BitBucket_Variations)
   return String;
   --  This function returns the name of the host bit bucket, e.g. /dev/null
   --  on posix systems.

   function Set_Arch    (value : String) return TArch;
   function Set_OSName  (value : String) return TOSName;
   function Set_Target  (value : String) return TTarget;
   function Set_Version (value : String) return TVersion;
   --  These functions essentially right-pad the unused characters with spaces


end DracoSystem;
