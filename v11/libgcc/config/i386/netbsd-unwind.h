/*
 * DWARF2 EH unwinding support for NetBSD: AMD x86-64 only.
 * Contributed by John Marino <gnugcc@marino.st>
 *
 * Do code reading to identify a signal frame, and set the frame
 * state data appropriately.  See unwind-dw2.c for the structs.
 */

#include <stdio.h>
#include <sys/ucontext.h>
#include <machine/frame.h>

#define REG_NAME(reg)   sf_uc.uc_mcontext.__gregs[_REG_## reg]

#ifdef __x86_64__
#define MD_FALLBACK_FRAME_STATE_FOR x86_64_netbsd_fallback_frame_state

static int
found_x86_64_signal_trampoline (unsigned int *pc)
{
/*
 * from "objdump -D /usr/lib/libc.a":
 * 
 * 0000000000000000 <__sigtramp_siginfo_2>:
 *    0:   4c 89 ff                mov    %r15,%rdi
 *    3:   48 c7 c0 34 01 00 00    mov    $0x134,%rax
 *    a:   0f 05                   syscall
 *    c:   48 c7 c7 ff ff ff ff    mov    $0xffffffffffffffff,%rdi
 *   13:   48 c7 c0 01 00 00 00    mov    $0x1,%rax
 *   1a:   0f 05                   syscall
 */

  if (   pc[0] == 0x48ff894c
      && pc[1] == 0x0134c0c7
      && pc[2] == 0x050f0000
      && pc[3] == 0xffc7c748
      && pc[4] == 0x48ffffff
      && pc[5] == 0x0001c0c7
      && pc[6] == 0x050f0000
  ) {
    return 1;
  }
  return 0;
}

static _Unwind_Reason_Code
x86_64_netbsd_fallback_frame_state (
  struct _Unwind_Context *context,
  _Unwind_FrameState *fs
) {
  /* signal_frame is sigframe_siginfo minus sf_ra handler return address */
  struct signal_frame {
     siginfo_t  sf_si;   /* actual saved siginfo  */
     ucontext_t sf_uc;   /* actual saved ucontext */
  };
  struct signal_frame *sf;
  long new_cfa;

  if (!found_x86_64_signal_trampoline (context->ra)) {
    return _URC_END_OF_STACK;
  }

  sf = (struct signal_frame *) context->cfa;
  new_cfa = sf->REG_NAME(RSP);
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = 7;
  fs->regs.cfa_offset = new_cfa - (long) context->cfa;

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

#else

  /* x86 is not supported */
# define MD_FALLBACK_FRAME_STATE_FOR x86_netbsd_fallback_frame_state

static _Unwind_Reason_Code
x86_netbsd_fallback_frame_state (
  __attribute__((unused)) struct _Unwind_Context *context,
  __attribute__((unused)) _Unwind_FrameState *fs
) {
    return _URC_END_OF_STACK;
}

#endif
