/* DWARF2 EH unwinding support for x86-freebsd
   Copyright (C) 2004, 2005, 2006, 2007, 2008
   Free Software Foundation, Inc.
   Copyright (C) 2010 AuroraUX (www.auroraux.org)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file with other programs, and to distribute
those programs without any restriction coming from the use of this
file.  (The General Public License restrictions do apply in other
respects; for example, they cover modification of the file, and
distribution when not linked into another program.)

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs. */

#include <signal.h>
#include <sys/ucontext.h>

#define MD_FALLBACK_FRAME_STATE_FOR x86_freebsd_fallback_frame_state

static _Unwind_Reason_Code
x86_freebsd_fallback_frame_state
(struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
    unsigned char *pc_ = context->ra;
    struct sigcontext *sc_;
    long new_cfa_;

    /* Standard sigcode */
    /*  movl $SYS_sigreturn, %eax; pushl %eax; int $0x80 */
    if (*(unsigned int *)(pc_+17) == 0x0001a1b8
	&& *(unsigned char *)(pc_+22) == 0x50
	&& *(unsigned short *)(pc_+23) == 0x80cd) {
      sc_ = context->cfa + 32;

    /* libpthread _thr_sig_handler */
    /* if ((sa_flags & SA_SIGINFO) != 0 || ... )
                        (*(sigfunc))(sig, info, ucp); */

    } else if (*(unsigned int *)(pc_-27) == 0xff6885f6
      && *(unsigned int *)(pc_-23) == 0x7540ffff
      && *(unsigned short *)(pc_) == 0xc483
      && *(unsigned char *)(pc_+2) == 0x10) {
      sc_ = context->cfa + 224;

    /* libpthread handle_signal */
    /* if ((shi->sa_flags & SA_SIGINFO) != 0 || ... )
                        (*(shi->sigfunc))(shi->sig, shi->info, shi->ucp); */

    } else if (*(unsigned int *)(pc_-26) == 0x400446f6
      && *(unsigned short *)(pc_-2) == 0x16ff
      && *(unsigned short *)(pc_) == 0xc483
      && *(unsigned char *)(pc_+2) == 0x10) {
      sc_ = context->cfa + 544;

    /* No match */
    } else {
      return _URC_END_OF_STACK;
    }

    new_cfa_ = sc_->sc_esp;
    fs->regs.cfa_how = CFA_REG_OFFSET;
    fs->regs.cfa_reg = __builtin_dwarf_sp_column ();
    fs->regs.cfa_offset = new_cfa_ - (long) context->cfa;

    /* The SVR4 register numbering macros aren't usable in libgcc.  */
    fs->regs.reg[0].how = REG_SAVED_OFFSET;
    fs->regs.reg[0].loc.offset = (long)&sc_->sc_eax - new_cfa_;
    fs->regs.reg[3].how = REG_SAVED_OFFSET;
    fs->regs.reg[3].loc.offset = (long)&sc_->sc_ebx - new_cfa_;
    fs->regs.reg[1].how = REG_SAVED_OFFSET;
    fs->regs.reg[1].loc.offset = (long)&sc_->sc_ecx - new_cfa_;
    fs->regs.reg[2].how = REG_SAVED_OFFSET;
    fs->regs.reg[2].loc.offset = (long)&sc_->sc_edx - new_cfa_;
    fs->regs.reg[6].how = REG_SAVED_OFFSET;
    fs->regs.reg[6].loc.offset = (long)&sc_->sc_esi - new_cfa_;
    fs->regs.reg[7].how = REG_SAVED_OFFSET;
    fs->regs.reg[7].loc.offset = (long)&sc_->sc_edi - new_cfa_;
    fs->regs.reg[5].how = REG_SAVED_OFFSET;
    fs->regs.reg[5].loc.offset = (long)&sc_->sc_ebp - new_cfa_;
    fs->regs.reg[8].how = REG_SAVED_OFFSET;
    fs->regs.reg[8].loc.offset = (long)&sc_->sc_eip - new_cfa_;
    fs->retaddr_column = 8;
    fs->signal_frame = 1;
    return _URC_NO_REASON;
}
