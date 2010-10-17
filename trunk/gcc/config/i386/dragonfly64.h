/* Definitions for AMD x86_64 running DragonFly BSD with ELF Format */

#undef  TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (x86-64 DragonFly/ELF)");

/* Tell final.c that we don't need a label passed to mcount.  */

#undef  MCOUNT_NAME
#define MCOUNT_NAME ".mcount"

#undef	SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
  { "dfbsd_dynamic_linker", DFBSD_DYNAMIC_LINKER }
