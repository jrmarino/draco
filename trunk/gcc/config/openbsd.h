/* Base configuration file for all OpenBSD targets.
   Copyright (C) 1999, 2000, 2004, 2005, 2007 Free Software Foundation, Inc.

   Copywrite (C) 2010 John Marino <draco@marino.st>

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



#undef SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR)     \
  (DEFAULT_SWITCH_TAKES_ARG (CHAR) \
   || (CHAR) == 'R')


/* Controlling the compilation driver.  */
/* TARGET_OS_CPP_BUILTINS() common to all OpenBSD targets.  */
#define OPENBSD_OS_CPP_BUILTINS()        \
  do						                    \
    {						                    \
      builtin_define ("__OpenBSD__");    \
      builtin_define ("__unix__");       \
      builtin_define ("__ANSI_COMPAT");  \
      builtin_assert ("system=unix");    \
      builtin_assert ("system=bsd");     \
      builtin_assert ("system=OpenBSD"); \
    }                                    \
  while (0)


#undef  CPP_SPEC
#define CPP_SPEC "%(cpp_cpu) %{posix:-D_POSIX_SOURCE} %{pthread:-D_POSIX_THREADS}"


/* As an elf system, we need crtbegin/crtend stuff.  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC "\
	%{!shared: %{pg:gcrt0%O%s} %{!pg:%{p:gcrt0%O%s} %{!p:crt0%O%s}} \
	crtbegin%O%s} %{shared:crtbeginS%O%s}"
#undef  STARTFILE_SPEC

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "%{!shared:crtend%O%s} %{shared:crtendS%O%s}"


#undef LIB_SPEC
#define LIB_SPEC "%{!shared:%{pthread:-lpthread} -lc}"


#undef LINK_SPEC
#define LINK_SPEC \
  "%{!shared:%{!nostdlib:%{!r*:%{!e*:-e __start}}}} \
   %{shared:-shared} %{R*} \
   %{static:-Bstatic} \
   %{!static:-Bdynamic} \
   %{assert*} \
   %{!dynamic-linker:-dynamic-linker %(obsd_dynamic_linker)}"


#define LINK_LIBGCC_SPEC "%D"

#define	OBSD_DYNAMIC_LINKER		"/usr/libexec/ld.so"


#if defined(HAVE_LD_EH_FRAME_HDR)
#define LINK_EH_SPEC "%{!static:--eh-frame-hdr} "
#endif


/* Use --as-needed -lgcc_s for eh support. */
#ifdef HAVE_LD_AS_NEEDED
#define USE_LD_AS_NEEDED 1
#endif


/* Since we use gas, stdin -> - is a good idea.  */
#define AS_NEEDS_DASH_FOR_PIPED_INPUT


/* explicitly allow execution from stack (yes, even on OpenBSD) */
#undef ENABLE_EXECUTE_STACK


/************************[  Target stuff  ]***********************************/

/* Draco / GNAT AUX only supports ELF for now */
#undef  OBJECT_FORMAT_ELF
#define OBJECT_FORMAT_ELF


/* Don't assume anything about the header files.  */
#undef  NO_IMPLICIT_EXTERN_C
#define NO_IMPLICIT_EXTERN_C	1


/* Code generation parameters.  */

/* Use periods rather than dollar signs in special g++ assembler names.
   This ensures the configuration knows our system correctly so we can link
   with libraries compiled with the native cc.  */
#undef NO_DOLLAR_IN_LABEL


