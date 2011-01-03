/* DWARF2 EH unwinding support for x86 NetBSD
   Copyright (C) 2010 John Marino (www.dragonlace.net) */

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs. */

#include <signal.h>
#include <sys/ucontext.h>
#include <machine/frame.h>

#define REG_NAME(reg)   sf_uc.uc_mcontext.__gregs[_REG_## reg]

#ifdef __x86_64__

/*   Add AMD64-specific code here   */

#else /* Next section is for i386  */

#define MD_FALLBACK_FRAME_STATE_FOR x86_netbsd_fallback_frame_state

static _Unwind_Reason_Code
x86_netbsd_fallback_frame_state
(struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  struct sigframe_siginfo *sf;
  long new_cfa;

  /*  We are looking for the following signal trampoline pattern. If we don't
      find it, we are at the end of the stack and can't unwind.

      <__sigtramp_siginfo_2+18>:  movl   $0xffffffff,0x4(%esp)
      <__sigtramp_siginfo_2+26>:  mov    $0x1,%eax
      <__sigtramp_siginfo_2+31>:  int    $0x80
  */

  if (   *(unsigned int   *) (context->ra + 18) == 0x042444c7
      && *(unsigned int   *) (context->ra + 22) == 0xffffffff
      && *(unsigned char  *) (context->ra + 26) == 0xb8
      && *(unsigned int   *) (context->ra + 27) == 0x00000001
      && *(unsigned short *) (context->ra + 31) == 0x80cd     )
  {
    sf = (struct sigframe_siginfo *) (context->cfa - 4);
    new_cfa = sf->REG_NAME(ESP);
    fs->regs.cfa_how = CFA_REG_OFFSET;
    fs->regs.cfa_reg = 4;
    fs->regs.cfa_offset = new_cfa - (long) context->cfa;
  }
  else
  {
    return _URC_END_OF_STACK;
  }

  /* The SVR4 register numbering macros aren't usable in libgcc.  */
  fs->regs.reg[0].how = REG_SAVED_OFFSET;
  fs->regs.reg[0].loc.offset = (long)&sf->REG_NAME(EAX) - new_cfa;
  fs->regs.reg[3].how = REG_SAVED_OFFSET;
  fs->regs.reg[3].loc.offset = (long)&sf->REG_NAME(EBX) - new_cfa;
  fs->regs.reg[1].how = REG_SAVED_OFFSET;
  fs->regs.reg[1].loc.offset = (long)&sf->REG_NAME(ECX) - new_cfa;
  fs->regs.reg[2].how = REG_SAVED_OFFSET;
  fs->regs.reg[2].loc.offset = (long)&sf->REG_NAME(EDX) - new_cfa;
  fs->regs.reg[6].how = REG_SAVED_OFFSET;
  fs->regs.reg[6].loc.offset = (long)&sf->REG_NAME(ESI) - new_cfa;
  fs->regs.reg[7].how = REG_SAVED_OFFSET;
  fs->regs.reg[7].loc.offset = (long)&sf->REG_NAME(EDI) - new_cfa;
  fs->regs.reg[5].how = REG_SAVED_OFFSET;
  fs->regs.reg[5].loc.offset = (long)&sf->REG_NAME(EBP) - new_cfa;
  fs->regs.reg[8].how = REG_SAVED_OFFSET;
  fs->regs.reg[8].loc.offset = (long)&sf->REG_NAME(EIP) - new_cfa;
  fs->retaddr_column = 8;
  fs->signal_frame = 1;
  return _URC_NO_REASON;
}
#endif /* ifdef __x86_64__  */
