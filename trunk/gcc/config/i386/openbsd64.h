/* Definitions for AMD x86_64 running OpenBSD BSD with ELF Format */

#undef  TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (x86-64 OpenBSD/ELF)");

#undef	SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
  { "obsd_dynamic_linker", OBSD_DYNAMIC_LINKER }

#undef  JUMP_TABLES_IN_TEXT_SECTION
#define JUMP_TABLES_IN_TEXT_SECTION (flag_pic)

