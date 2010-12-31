/* Definitions for AMD x86-64 running FreeBSD with ELF format
   Copyright (C) 2002, 2004, 2007 Free Software Foundation, Inc.
   Contributed by David O'Brien <obrien@FreeBSD.org>
   Copyright (C) 2010 John Marino <draco@marino.st>

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


#undef  TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (FreeBSD/x86-64 ELF)");

#define SUBTARGET_EXTRA_SPECS \
  { "fbsd_dynamic_linker", FBSD_DYNAMIC_LINKER }

/* Provide a LINK_SPEC appropriate for the FreeBSD/x86-64 ELF target.
   This is a copy of LINK_SPEC from <i386/freebsd.h> tweaked for
   the x86-64 target.  */

#undef	LINK_SPEC
#define LINK_SPEC "\
  %{m32:-m elf_i386_fbsd} \
  %{m64:-m elf_x86_64_fbsd} \
  %{v:-V} \
  %{assert*} %{R*} %{rpath*} %{defsym*} \
  %{shared:-Bshareable %{h*} %{soname*}} \
    %{!shared: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
	%{!dynamic-linker:-dynamic-linker %(fbsd_dynamic_linker) }} \
    %{static:-Bstatic}} \
  %{symbolic:-Bsymbolic}"


/* Through FreeBSD 8.2 at least, stack is denied execution rights by libthr 
   This only affects AMD64 since i386 ignores the nx bit (non-PAE) 
   perms = PROT_READ | PROT_WRITE | PROT_EXEC;
*/
#define ENABLE_EXECUTE_STACK                             \
extern void __enable_execute_stack (void *);             \
void                                                     \
__enable_execute_stack (void *addr)                      \
{                                                        \
  extern int getpagesize (void);                         \
  extern int mprotect (void *, size_t, int);             \
                                                         \
  static int size;                                       \
  long mask;                                             \
  char *page, *ends;                                     \
  long page_addr = (long) addr;                          \
  long ends_addr = (long) (addr + TRAMPOLINE_SIZE);      \
  int  perms     = 7;                                    \
                                                         \
  if (size == 0)                                         \
  {                                                      \
    size = getpagesize();                                \
  }                                                      \
  mask = ~((long) size - 1);                             \
  page = (char *)  (page_addr & mask);                   \
  ends = (char *) ((ends_addr & mask) + size);           \
  (void) mprotect (page, ends - page, perms);            \
}
