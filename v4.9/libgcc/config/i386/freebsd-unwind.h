/* DWARF2 EH unwinding support for FreeBSD: AMD x86-64 and x86.
 *  Copyright (C) 2010, 2012, 2014 John Marino <draco@marino.st>
 *
 *  Do code reading to identify a signal frame, and set the frame
 *  state data appropriately.  See unwind-dw2.c for the structs.
 */

#include <sys/types.h>
#include <signal.h>
#include <sys/ucontext.h>
#include <machine/sigframe.h>

#define REG_NAME(reg)	sf_uc.uc_mcontext.mc_## reg

#ifdef __x86_64__
#define MD_FALLBACK_FRAME_STATE_FOR x86_64_freebsd_fallback_frame_state

#if (__FreeBSD__ < 9)
#include <sys/sysctl.h>
static void
x86_64_sigtramp_range (unsigned char **start, unsigned char **end)
{
  unsigned long ps_strings;
  int mib[2];
  size_t len;

  mib[0] = CTL_KERN;
  mib[1] = KERN_PS_STRINGS;
  len = sizeof (ps_strings);
  sysctl (mib, 2, &ps_strings, &len, NULL, 0);

  *start = (unsigned char *)ps_strings - 32;
  *end   = (unsigned char *)ps_strings;
}
#endif


static _Unwind_Reason_Code
x86_64_freebsd_fallback_frame_state
(struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  struct sigframe *sf;
  long new_cfa;

#if (__FreeBSD__ < 9)
  unsigned char *pc = context->ra;
  unsigned char *sigtramp_start, *sigtramp_end;

  x86_64_sigtramp_range(&sigtramp_start, &sigtramp_end);
  if (pc >= sigtramp_end || pc < sigtramp_start)
    return _URC_END_OF_STACK;
#else
  /* Prior to FreeBSD 9, the signal trampoline was located immediately
     before the ps_strings.  To support non-executable stacks on AMD64,
     the sigtramp was moved to a shared page for FreeBSD 9.  We are
     stuck looking for frame patterns again (sys/amd64/amd64/sigtramp.S):

     <pc + 00>:  lea     0x10(%rsp),%rdi
     <pc + 05>:  pushq   $0x0
     <pc + 17>:  mov     $0x1a1,%rax
     <pc + 14>:  syscall

     If we can't find this pattern, we're at the end of the stack.
  */

  if (!(   *(unsigned int *)(context->ra)      == 0x247c8d48
        && *(unsigned int *)(context->ra +  4) == 0x48006a10
        && *(unsigned int *)(context->ra +  8) == 0x01a1c0c7
        && *(unsigned int *)(context->ra + 12) == 0x050f0000 ))
    return _URC_END_OF_STACK;
#endif

  sf = (struct sigframe *) context->cfa;
  new_cfa = sf->REG_NAME(rsp);
  fs->regs.cfa_how = CFA_REG_OFFSET;
  /* Register 7 is rsp  */
  fs->regs.cfa_reg = 7;
  fs->regs.cfa_offset = new_cfa - (long) context->cfa;

  /* The SVR4 register numbering macros aren't usable in libgcc.  */
  fs->regs.reg[0].how = REG_SAVED_OFFSET;
  fs->regs.reg[0].loc.offset = (long)&sf->REG_NAME(rax) - new_cfa;
  fs->regs.reg[1].how = REG_SAVED_OFFSET;
  fs->regs.reg[1].loc.offset = (long)&sf->REG_NAME(rdx) - new_cfa;
  fs->regs.reg[2].how = REG_SAVED_OFFSET;
  fs->regs.reg[2].loc.offset = (long)&sf->REG_NAME(rcx) - new_cfa;
  fs->regs.reg[3].how = REG_SAVED_OFFSET;
  fs->regs.reg[3].loc.offset = (long)&sf->REG_NAME(rbx) - new_cfa;
  fs->regs.reg[4].how = REG_SAVED_OFFSET;
  fs->regs.reg[4].loc.offset = (long)&sf->REG_NAME(rsi) - new_cfa;
  fs->regs.reg[5].how = REG_SAVED_OFFSET;
  fs->regs.reg[5].loc.offset = (long)&sf->REG_NAME(rdi) - new_cfa;
  fs->regs.reg[6].how = REG_SAVED_OFFSET;
  fs->regs.reg[6].loc.offset = (long)&sf->REG_NAME(rbp) - new_cfa;
  fs->regs.reg[8].how = REG_SAVED_OFFSET;
  fs->regs.reg[8].loc.offset = (long)&sf->REG_NAME(r8) - new_cfa;
  fs->regs.reg[9].how = REG_SAVED_OFFSET;
  fs->regs.reg[9].loc.offset = (long)&sf->REG_NAME(r9) - new_cfa;
  fs->regs.reg[10].how = REG_SAVED_OFFSET;
  fs->regs.reg[10].loc.offset = (long)&sf->REG_NAME(r10) - new_cfa;
  fs->regs.reg[11].how = REG_SAVED_OFFSET;
  fs->regs.reg[11].loc.offset = (long)&sf->REG_NAME(r11) - new_cfa;
  fs->regs.reg[12].how = REG_SAVED_OFFSET;
  fs->regs.reg[12].loc.offset = (long)&sf->REG_NAME(r12) - new_cfa;
  fs->regs.reg[13].how = REG_SAVED_OFFSET;
  fs->regs.reg[13].loc.offset = (long)&sf->REG_NAME(r13) - new_cfa;
  fs->regs.reg[14].how = REG_SAVED_OFFSET;
  fs->regs.reg[14].loc.offset = (long)&sf->REG_NAME(r14) - new_cfa;
  fs->regs.reg[15].how = REG_SAVED_OFFSET;
  fs->regs.reg[15].loc.offset = (long)&sf->REG_NAME(r15) - new_cfa;
  fs->regs.reg[16].how = REG_SAVED_OFFSET;
  fs->regs.reg[16].loc.offset = (long)&sf->REG_NAME(rip) - new_cfa;
  fs->retaddr_column = 16;
  fs->signal_frame = 1;
  return _URC_NO_REASON;
}

#else /* Next section is for i386  */

#define MD_FALLBACK_FRAME_STATE_FOR x86_freebsd_fallback_frame_state

/*
 * We can't use KERN_PS_STRINGS anymore if we want to support FreeBSD32
 * compat on AMD64.  The sigtramp is in a shared page in that case so the
 * x86_sigtramp_range only works on a true i386 system.  We have to
 * search for the sigtramp frame if we want it working everywhere.

#include <sys/sysctl.h>
static void
x86_sigtramp_range (unsigned char **start, unsigned char **end)
{
  unsigned long ps_strings;
  int mib[2];
  size_t len;

  mib[0] = CTL_KERN;
  mib[1] = KERN_PS_STRINGS;
  len = sizeof (ps_strings);
  sysctl (mib, 2, &ps_strings, &len, NULL, 0);

  *start = (unsigned char *)ps_strings - 128;
  *end   = (unsigned char *)ps_strings;
}
*/


static _Unwind_Reason_Code
x86_freebsd_fallback_frame_state
(struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  struct sigframe *sf;
  long new_cfa;

/*
 * i386 sigtramp frame we are looking for follows.
 * Apparently PSL_VM is variable, so we can't look past context->ra + 4
 * <sigcode>:
 *   0:	ff 54 24 10          	call   *0x10(%esp)          *SIGF_HANDLER
 *   4:	8d 44 24 20          	lea    0x20(%esp),%eax       SIGF_UC
 *   8:	50                   	push   %eax
 *   9:	f7 40 54 ?? ?? ?? ?? 	testl  $PSL_VM,0x54(%eax)
 *  10:	75 03                	jne    15 <sigcode+0x15>
 *  12:	8e 68 14             	mov    0x14(%eax),%gs        UC_GS
 *  15:	a1 a1 01 00 00       	mov    0x1a1,%eax           $SYS_sigreturn
 */

  if (!(   *(unsigned int *)(context->ra - 4) == 0x102454ff
        && *(unsigned int *)(context->ra)     == 0x2024448d ))
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
#endif /* ifdef __x86_64__  */
