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


#undef  TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()         \
  do                                     \
    {                                    \
      builtin_define ("__OpenBSD__");    \
      builtin_define ("__unix__");       \
      builtin_define ("__ANSI_COMPAT");  \
      builtin_define ("__ELF__");        \
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

/* Under OpenBSD, the normal location of the various *crt*.o files is the
   /usr/lib directory.  */
#undef  STANDARD_STARTFILE_PREFIX
#define STANDARD_STARTFILE_PREFIX	"/usr/local/lib/"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "%{!shared:crtend%O%s} %{shared:crtendS%O%s}"


#undef  LIB_SPEC
#define LIB_SPEC "%{!shared:%{pthread:-lpthread} -lc}"


#undef  LINK_SPEC
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

/* Make gree agree with OpenBSD's standard headers <machine/_types.h>  */

#undef  WCHAR_TYPE
#define WCHAR_TYPE "int"

/* Code generation parameters.  */

/* Use periods rather than dollar signs in special g++ assembler names.
   This ensures the configuration knows our system correctly so we can link
   with libraries compiled with the native cc.  */
#undef NO_DOLLAR_IN_LABEL


/* bug work around: we don't want to support #pragma weak, but the current
   code layout needs HANDLE_PRAGMA_WEAK asserted for __attribute((weak)) to
   work.  On the other hand, we don't define HANDLE_PRAGMA_WEAK directly,
   as this depends on a few other details as well...  */
#define HANDLE_SYSV_PRAGMA 1


/* OpenBSD assembler is hacked to have .type & .size support even in a.out
   format object files.  Functions size are supported but not activated 
   yet (look for GRACE_PERIOD_EXPIRED in gas/config/obj-aout.c).  
   SET_ASM_OP is needed for attribute alias to work.  */
         
#undef  TYPE_ASM_OP
#define TYPE_ASM_OP     "\t.type\t"

#undef  SIZE_ASM_OP
#define SIZE_ASM_OP     "\t.size\t"

#undef  SET_ASM_OP
#define SET_ASM_OP      "\t.set\t"

#undef GLOBAL_ASM_OP
#define GLOBAL_ASM_OP   "\t.globl\t"
  
  
/* The following macro defines the format used to output the second
   operand of the .type assembler directive.  */
#undef  TYPE_OPERAND_FMT
#define TYPE_OPERAND_FMT  "@%s"

/* These macros generate the special .type and .size directives which
   are used to set the corresponding fields of the linker symbol table
   entries under OpenBSD.  These macros also have to output the starting 
   labels for the relevant functions/objects.  */  
   
/* Extra assembler code needed to declare a function properly.
   Some assemblers may also need to also have something extra said 
   about the function's return value.  We allow for that here.  */
#undef  ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
  do {									\
    ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "function");			\
    ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));			\
    ASM_OUTPUT_FUNCTION_LABEL (FILE, NAME, DECL);			\
  } while (0)
         

/* Declare the size of a function.  */
#undef  ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)		\
  do {								\
    if (!flag_inhibit_size_directive)				\
      ASM_OUTPUT_MEASURED_SIZE (FILE, FNAME);			\
  } while (0)
  
/* Extra assembler code needed to declare an object properly.  */
#undef  ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)      \
  do {                                                 \
    HOST_WIDE_INT size;                                \
    ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "object");  \
    size_directive_output = 0;                         \
    if (!flag_inhibit_size_directive                   \
        && (DECL) && DECL_SIZE (DECL))                 \
      {                                                \
        size_directive_output = 1;                     \
        size = int_size_in_bytes (TREE_TYPE (DECL));   \
        ASM_OUTPUT_SIZE_DIRECTIVE (FILE, NAME, size);  \
      }                                                \
      ASM_OUTPUT_LABEL (FILE, NAME);                   \
  } while (0)
  
/* Output the size directive for a decl in rest_of_decl_compilation
   in the case where we did not do so before the initializer.
   Once we find the error_mark_node, we know that the value of
   size_directive_output was set by ASM_DECLARE_OBJECT_NAME 
   when it was run for the same decl.  */
#undef  ASM_FINISH_DECLARE_OBJECT
#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP_LEVEL, AT_END)  \
  do {                                                            \
    const char *name = XSTR (XEXP (DECL_RTL (DECL), 0), 0);       \
    HOST_WIDE_INT size;                                           \
    if (!flag_inhibit_size_directive && DECL_SIZE (DECL)          \
        && ! AT_END && TOP_LEVEL                                  \
        && DECL_INITIAL (DECL) == error_mark_node                 \
        && !size_directive_output)                                \
      {                                                           \
        size_directive_output = 1;                                \
        size = int_size_in_bytes (TREE_TYPE (DECL));              \
        ASM_OUTPUT_SIZE_DIRECTIVE (FILE, name, size);             \
      }                                                           \
  } while (0)
  
  

/* Those are eneric' ways to weaken/globalize a label. We shouldn't need
   to override a processor specific definition. Hence, #ifndef ASM_*
   In case overriding turns out to be needed, one can always #undef ASM_* 
   before including this file.  */
         
/* Tell the assembler that a symbol is weak.  
   Note: netbsd arm32 assembler needs a .globl here. An override may 
   be needed when/if we go for arm32 support.  */
#ifndef ASM_WEAKEN_LABEL
#define ASM_WEAKEN_LABEL(FILE,NAME) \
  do {fputs ("\t.weak\t", FILE); assemble_name (FILE, NAME); \
      fputc ('\n', FILE); } while (0)
#endif  
