/* Definitions for Intel 386 running DragonFly with ELF format
   Copyright (C) 1996, 2000, 2002, 2004, 2007 Free Software Foundation, Inc.
   Contributed by Eric Youngdale.
   Modified for stabs-in-ELF by H.J. Lu.
   Adapted from GNU/Linux version by John Polstra.
   Continued development by David O'Brien <obrien@freebsd.org>
   Copyright (C) 2010 AuroraUX (www.auroraux.org)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#undef DBX_REGISTER_NUMBER
#undef WCHAR_TYPE_SIZE
#undef PTRDIFF_TYPE
#undef SIZE_TYPE
#undef LINK_SPEC

#if TARGET_64BIT

#define TARGET_VERSION fprintf (stderr, " (x86-64 DragonFly/ELF)");
#define DBX_REGISTER_NUMBER(n) dbx64_register_map[n]
#define WCHAR_TYPE_SIZE	32
#define PTRDIFF_TYPE	"long int"
#define SIZE_TYPE	"long unsigned int"
#define LINK_SPEC "\
  %{v:-V} \
  %{assert*} %{R*} %{rpath*} %{defsym*} \
  %{shared:-Bshareable %{h*} %{soname*}} \
    %{!shared: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
	%{!dynamic-linker:-dynamic-linker %(dfbsd_dynamic_linker) }} \
    %{static:-Bstatic}} \
  %{symbolic:-Bsymbolic}"

#else /* 32-bit arch */

#define TARGET_VERSION fprintf (stderr, " (i386 DragonFly/ELF)");
#define DBX_REGISTER_NUMBER(n) svr4_dbx_register_map[n]
#define WCHAR_TYPE_SIZE	BITS_PER_WORD
#define PTRDIFF_TYPE	"int"
#define SIZE_TYPE	"unsigned int"
#define LINK_SPEC "\
  %{p:%nconsider using `-pg' instead of `-p' with gprof(1)} \
  %{v:-V} \
  %{assert*} %{R*} %{rpath*} %{defsym*} \
  %{shared:-Bshareable %{h*} %{soname*}} \
    %{!shared: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
        %{!dynamic-linker:-dynamic-linker %(dfbsd_dynamic_linker) }} \
    %{static:-Bstatic}} \
  %{symbolic:-Bsymbolic}"

#endif /* TAIL: TARGET_64BIT */



/* Override the default comment-starter of "/".  */
#undef  ASM_COMMENT_START
#define ASM_COMMENT_START "#"

#undef  ASM_APP_ON
#define ASM_APP_ON "#APP\n"

#undef  ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

#undef  NO_PROFILE_COUNTERS
#define NO_PROFILE_COUNTERS	1

/* Tell final.c that we don't need a label passed to mcount.  */

#undef  MCOUNT_NAME
#define MCOUNT_NAME ".mcount"

/* mcount may clobber caller-saved registers, so ... */
#undef  MCOUNT_PRESERVES_ALL_REGS
#define MCOUNT_PRESERVES_ALL_REGS 0


#undef  SUBTARGET_EXTRA_SPECS	/* i386.h bogusly defines it.  */
#define SUBTARGET_EXTRA_SPECS \
  { "dfbsd_dynamic_linker", DFBSD_DYNAMIC_LINKER }
    
/* Provide a STARTFILE_SPEC appropriate for DragonFly.  Here we add
   the magical crtbegin.o file (see crtstuff.c) which provides part 
	of the support for getting C++ file-scope static object constructed 
	before entering `main'.  */
   
#undef	STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared: \
     %{pg:gcrt1.o%s} %{!pg:%{p:gcrt1.o%s} \
		       %{!p:%{profile:gcrt1.o%s} \
			 %{!profile:crt1.o%s}}}} \
   crti.o%s %{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}"

/* Provide a ENDFILE_SPEC appropriate for DragonFly.  Here we tack on
   the magical crtend.o file (see crtstuff.c) which provides part of 
	the support for getting C++ file-scope static object constructed 
	before entering `main', followed by a normal "finalizer" file, 
	`crtn.o'.  */

#undef	ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{!shared:crtend.o%s} %{shared:crtendS.o%s} crtn.o%s"


/* A C statement to output to the stdio stream FILE an assembler
   command to advance the location counter to a multiple of 1<<LOG
   bytes if it is within MAX_SKIP bytes.

   This is used to align code labels according to Intel recommendations.  */

#ifdef HAVE_GAS_MAX_SKIP_P2ALIGN
#undef  ASM_OUTPUT_MAX_SKIP_ALIGN
#define ASM_OUTPUT_MAX_SKIP_ALIGN(FILE, LOG, MAX_SKIP)					\
  if ((LOG) != 0) {														\
    if ((MAX_SKIP) == 0) fprintf ((FILE), "\t.p2align %d\n", (LOG));	\
    else fprintf ((FILE), "\t.p2align %d,,%d\n", (LOG), (MAX_SKIP));	\
  }
#endif

/* Don't default to pcc-struct-return, we want to retain compatibility with
   older gcc versions AND pcc-struct-return is nonreentrant.
   (even though the SVR4 ABI for the i386 says that records and unions are
   returned in memory).  */

#undef  DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 0

/* DragonFly sets the rounding precision of the FPU to 53 bits, but GNAT
   resets it to full precision.  */
#undef TARGET_96_ROUND_53_LONG_DOUBLE
#define TARGET_96_ROUND_53_LONG_DOUBLE 0

/* Define this to be nonzero if static stack checking is supported. */
#define STACK_CHECK_STATIC_BUILTIN 1

/* Define location of OS-specific unwind support configuration. */
#define MD_UNWIND_SUPPORT "config/i386/dragonfly-unwind.h"
