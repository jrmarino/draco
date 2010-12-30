/* DWARF2 EH unwinding support for x86 OpenBSD
   Copyright (C) 2010 John Marino (www.dragonlace.net) */

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs. */

#include <sys/param.h>
#include <sys/sysctl.h>
#include <machine/frame.h>
/* machine/frame.h brings in sys/signal.h
   sys/signal.h    brings in machine/signal.h  (sigcontext)
                   brings in sys/siginfo.h     (siginfo_t) */


#define REG_NAME(reg)                 sf_sc.sc_## reg
#define MD_FALLBACK_FRAME_STATE_FOR   x86_openbsd_fallback_frame_state


static void
x86_sigtramp_range (unsigned char **start, unsigned char **end)
{
  struct _ps_strings ps_strings;
  int mib[2];
  size_t len;

  mib[0] = CTL_VM;
  mib[1] = VM_PSSTRINGS;
  len = sizeof (ps_strings);
  sysctl (mib, 2, &ps_strings, &len, NULL, 0);

  *start = (unsigned char *)ps_strings.val - 128;
  *end   = (unsigned char *)ps_strings.val;
}


static _Unwind_Reason_Code
x86_openbsd_fallback_frame_state
(struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  unsigned char *pc = context->ra;
  unsigned char *sigtramp_start, *sigtramp_end;
  struct sigframe *sf;
  long new_cfa;

  x86_sigtramp_range(&sigtramp_start, &sigtramp_end);

  if (pc >= sigtramp_end || pc < sigtramp_start)
    return _URC_END_OF_STACK;

  sf = (struct sigframe *) context->cfa;
  new_cfa = sf->REG_NAME(esp);

  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = 4;
  fs->regs.cfa_offset = new_cfa - (long) context->cfa;

  /* The SVR4 register numbering macros aren't usable in libgcc.  */
  fs->regs.reg[0].how = REG_SAVED_OFFSET;
  fs->regs.reg[0].loc.offset = (long)&sf->REG_NAME(eax) - new_cfa;
  fs->regs.reg[3].how = REG_SAVED_OFFSET;
  fs->regs.reg[3].loc.offset = (long)&sf->REG_NAME(ebx) - new_cfa;
  fs->regs.reg[1].how = REG_SAVED_OFFSET;
  fs->regs.reg[1].loc.offset = (long)&sf->REG_NAME(ecx) - new_cfa;
  fs->regs.reg[2].how = REG_SAVED_OFFSET;
  fs->regs.reg[2].loc.offset = (long)&sf->REG_NAME(edx) - new_cfa;
  fs->regs.reg[6].how = REG_SAVED_OFFSET;
  fs->regs.reg[6].loc.offset = (long)&sf->REG_NAME(esi) - new_cfa;
  fs->regs.reg[7].how = REG_SAVED_OFFSET;
  fs->regs.reg[7].loc.offset = (long)&sf->REG_NAME(edi) - new_cfa;
  fs->regs.reg[5].how = REG_SAVED_OFFSET;
  fs->regs.reg[5].loc.offset = (long)&sf->REG_NAME(ebp) - new_cfa;
  fs->regs.reg[8].how = REG_SAVED_OFFSET;
  fs->regs.reg[8].loc.offset = (long)&sf->REG_NAME(eip) - new_cfa;
  fs->retaddr_column = 8;
  fs->signal_frame = 1;
  return _URC_NO_REASON;
}
