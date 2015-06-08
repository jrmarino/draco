/* DWARF2 EH unwinding support for NetBSD: AMD x86-64 and x86.
   Copyright (C) 2015 Free Software Foundation, Inc.
   Contributed by John Marino <gnugcc@marino.st>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs. */

#include <sys/ucontext.h>
#include <machine/frame.h>

#define REG_NAME(reg)   sf_uc.uc_mcontext.__gregs[_REG_## reg]

#ifdef __x86_64__

#define MD_FALLBACK_FRAME_STATE_FOR x86_64_netbsd_fallback_frame_state

static _Unwind_Reason_Code
x86_64_netbsd_fallback_frame_state
(struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  /* signal_frame is sigframe_siginfo minus sf_ra handler return address */
  struct signal_frame {
         siginfo_t    sf_si;   /* actual saved siginfo  */
         ucontext_t   sf_uc;   /* actual saved ucontext */
  };
  struct signal_frame *sf;
  long new_cfa;

  /*  We are looking for the following signal trampoline pattern. If we don't
      find it, we are at the end of the stack and can't unwind.

      <__sigtramp_siginfo_2+12>:  mov     $0xffffffffffffffff,%rdi
      <__sigtramp_siginfo_2+19>:  mov     $0x1,%rax
      <__sigtramp_siginfo_2+26>:  syscall
  */

  if (   *(unsigned int   *) (context->ra + 12) == 0xffc7c748
      && *(unsigned int   *) (context->ra + 16) == 0x48ffffff
      && *(unsigned int   *) (context->ra + 20) == 0x0001c0c7
      && *(unsigned int   *) (context->ra + 24) == 0x050f0000 )
  {
    sf = (struct signal_frame *) context->cfa;
    new_cfa = sf->REG_NAME(RSP);
    fs->regs.cfa_how = CFA_REG_OFFSET;
    fs->regs.cfa_reg = 7;
    fs->regs.cfa_offset = new_cfa - (long) context->cfa;
  }
  else
  {
    return _URC_END_OF_STACK;
  }

  /* The SVR4 register numbering macros aren't usable in libgcc.  */
  fs->regs.reg[ 0].how = REG_SAVED_OFFSET;
  fs->regs.reg[ 0].loc.offset = (long)&sf->REG_NAME(RAX) - new_cfa;
  fs->regs.reg[ 1].how = REG_SAVED_OFFSET;
  fs->regs.reg[ 1].loc.offset = (long)&sf->REG_NAME(RDX) - new_cfa;
  fs->regs.reg[ 2].how = REG_SAVED_OFFSET;
  fs->regs.reg[ 2].loc.offset = (long)&sf->REG_NAME(RCX) - new_cfa;
  fs->regs.reg[ 3].how = REG_SAVED_OFFSET;
  fs->regs.reg[ 3].loc.offset = (long)&sf->REG_NAME(RBX) - new_cfa;
  fs->regs.reg[ 4].how = REG_SAVED_OFFSET;
  fs->regs.reg[ 4].loc.offset = (long)&sf->REG_NAME(RSI) - new_cfa;
  fs->regs.reg[ 5].how = REG_SAVED_OFFSET;
  fs->regs.reg[ 5].loc.offset = (long)&sf->REG_NAME(RDI) - new_cfa;
  fs->regs.reg[ 6].how = REG_SAVED_OFFSET;
  fs->regs.reg[ 6].loc.offset = (long)&sf->REG_NAME(RBP) - new_cfa;
  fs->regs.reg[ 8].how = REG_SAVED_OFFSET;
  fs->regs.reg[ 8].loc.offset = (long)&sf->REG_NAME(R8)  - new_cfa;
  fs->regs.reg[ 9].how = REG_SAVED_OFFSET;
  fs->regs.reg[ 9].loc.offset = (long)&sf->REG_NAME(R9)  - new_cfa;
  fs->regs.reg[10].how = REG_SAVED_OFFSET;
  fs->regs.reg[10].loc.offset = (long)&sf->REG_NAME(R10) - new_cfa;
  fs->regs.reg[11].how = REG_SAVED_OFFSET;
  fs->regs.reg[11].loc.offset = (long)&sf->REG_NAME(R11) - new_cfa;
  fs->regs.reg[12].how = REG_SAVED_OFFSET;
  fs->regs.reg[12].loc.offset = (long)&sf->REG_NAME(R12) - new_cfa;
  fs->regs.reg[13].how = REG_SAVED_OFFSET;
  fs->regs.reg[13].loc.offset = (long)&sf->REG_NAME(R13) - new_cfa;
  fs->regs.reg[14].how = REG_SAVED_OFFSET;
  fs->regs.reg[14].loc.offset = (long)&sf->REG_NAME(R14) - new_cfa;
  fs->regs.reg[15].how = REG_SAVED_OFFSET;
  fs->regs.reg[15].loc.offset = (long)&sf->REG_NAME(R15) - new_cfa;
  fs->regs.reg[16].how = REG_SAVED_OFFSET;
  fs->regs.reg[16].loc.offset = (long)&sf->REG_NAME(RIP) - new_cfa;
  fs->retaddr_column = 16;
  fs->signal_frame = 1;
  return _URC_NO_REASON;
}

#else /* Next section is for i386  */

#define MD_FALLBACK_FRAME_STATE_FOR x86_netbsd_fallback_frame_state

static _Unwind_Reason_Code
x86_netbsd_fallback_frame_state
(struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  /* signal_frame is sigframe_siginfo minus sf_ra handler return address */
  struct signal_frame {
         int          sf_signum;  /* "signum" argument for handler" */
         siginfo_t   *sf_sip;     /* "sip"    argument for handler" */
         ucontext_t  *sf_ucp;     /* "ucp"    argument for handler" */
         siginfo_t    sf_si;      /* actual saved siginfo  */
         ucontext_t   sf_uc;      /* actual saved ucontext */
  };
  struct signal_frame *sf;
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
    sf = (struct signal_frame *) context->cfa;
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
