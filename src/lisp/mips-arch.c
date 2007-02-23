/*

 $Header: /project/cmucl/cvsroot/src/lisp/mips-arch.c,v 1.10 2005/09/15 18:26:52 rtoy Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>

#ifdef mach
#include <mips/cpu.h>
#else
#ifdef irix
#include <sys/sbd.h>
#endif
#endif

#include "lisp.h"
#include "globals.h"
#include "validate.h"
#include "os.h"
#include "internals.h"
#include "arch.h"
#include "lispregs.h"
#include "signal.h"
#include "alloc.h"
#include "interrupt.h"
#include "interr.h"
#include "breakpoint.h"

char *
arch_init(void)
{
    return NULL;
}

os_vm_address_t
arch_get_bad_addr(int sig, int code, struct sigcontext * scp)
{
    /* Finding the bad address on the mips is easy. */
    return (os_vm_address_t) scp->sc_badvaddr;
}

#ifdef irix
void
emulate_branch(struct sigcontext *scp, unsigned long inst)
{
    long opcode = inst >> 26;
    long r1 = (inst >> 21) & 0x1f;
    long r2 = (inst >> 16) & 0x1f;
    long bdisp = (inst & (1 << 15)) ? inst | (-1 << 16) : inst & 0xffff;
    long jdisp = (inst & (1 << 25)) ? inst | (-1 << 26) : inst & 0xffff;
    long disp = 0;

    switch (opcode) {
      case 0x1:		/* bltz, bgez, bltzal, bgezal */
	  switch ((inst >> 16) & 0x1f) {
	    case 0x00:		/* bltz */
		if (scp->sc_regs[r1] < 0)
		    disp = bdisp;
		break;
	    case 0x01:		/* bgez */
		if (scp->sc_regs[r1] >= 0)
		    disp = bdisp;
		break;
	    case 0x10:		/* bltzal */
		if (scp->sc_regs[r1] < 0)
		    disp = bdisp;
		scp->sc_regs[31] = scp->sc_pc + 4;
		break;
	    case 0x11:		/* bgezal */
		if (scp->sc_regs[r1] >= 0)
		    disp = bdisp;
		scp->sc_regs[31] = scp->sc_pc + 4;
		break;
	  }
	  break;
      case 0x4:		/* beq */
	  if (scp->sc_regs[r1] == scp->sc_regs[r2])
	      disp = bdisp;
	  break;
      case 0x5:		/* bne */
	  if (scp->sc_regs[r1] != scp->sc_regs[r2])
	      disp = bdisp;
	  break;
      case 0x6:		/* ble */
	  if (scp->sc_regs[r1] <= scp->sc_regs[r2])
	      disp = bdisp;
	  break;
      case 0x7:		/* bgtz */
	  if (scp->sc_regs[r1] >= scp->sc_regs[r2])
	      disp = bdisp;
	  break;
      case 0x2:		/* j */
	  disp = jdisp;
	  break;
      case 0x3:		/* jal */
	  disp = jdisp;
	  scp->sc_regs[31] = scp->sc_pc + 4;
	  break;
    }
    scp->sc_pc += disp * 4;
}
#endif

void
arch_skip_instruction(scp)
     struct sigcontext *scp;
{
    /* Skip the offending instruction */
    if (scp->sc_cause & CAUSE_BD)
	emulate_branch(scp, *(unsigned long *) scp->sc_pc);
    else
	scp->sc_pc += 4;
}

unsigned char *
arch_internal_error_arguments(struct sigcontext *scp)
{
    if (scp->sc_cause & CAUSE_BD)
	return (unsigned char *) (scp->sc_pc + 8);
    else
	return (unsigned char *) (scp->sc_pc + 4);
}

boolean
arch_pseudo_atomic_atomic(struct sigcontext *scp)
{
    return (scp->sc_regs[reg_ALLOC] & 1);
}

#define PSEUDO_ATOMIC_INTERRUPTED_BIAS 0x7f000000

void
arch_set_pseudo_atomic_interrupted(struct sigcontext *scp)
{
    scp->sc_regs[reg_NL4] += PSEUDO_ATOMIC_INTERRUPTED_BIAS;
}

unsigned long
arch_install_breakpoint(void *pc)
{
    unsigned long *ptr = (unsigned long *) pc;
    unsigned long result = *ptr;

    *ptr = (trap_Breakpoint << 16) | 0xd;

    os_flush_icache((os_vm_address_t) ptr, sizeof(unsigned long));

    return result;
}

void
arch_remove_breakpoint(void *pc, unsigned long orig_inst)
{
    *(unsigned long *) pc = orig_inst;

    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned long));
}

static unsigned long *skipped_break_addr, displaced_after_inst;
static int orig_sigmask;

void
arch_do_displaced_inst(struct sigcontext *scp, unsigned long orig_inst)
{
    unsigned long *pc = (unsigned long *) scp->sc_pc;
    unsigned long *break_pc, *next_pc;
    unsigned long next_inst;
    int opcode;
    struct sigcontext tmp;

    orig_sigmask = scp->sc_mask;
    scp->sc_mask = BLOCKABLE;

    /* Figure out where the breakpoint is, and what happens next. */
    if (scp->sc_cause & CAUSE_BD) {
	break_pc = pc + 1;
	next_inst = *pc;
    } else {
	break_pc = pc;
	next_inst = orig_inst;
    }

    /* Put the original instruction back. */
    *break_pc = orig_inst;
    os_flush_icache((os_vm_address_t) break_pc, sizeof(unsigned long));

    skipped_break_addr = break_pc;

    /* Figure out where it goes. */
    opcode = next_inst >> 26;
    if (opcode == 1 || ((opcode & 0x3c) == 0x4)
	|| ((next_inst & 0xf00e0000) == 0x80000000)) {
	tmp = *scp;
	emulate_branch(&tmp, next_inst);
	next_pc = (unsigned long *) tmp.sc_pc;
    } else
	next_pc = pc + 1;

    displaced_after_inst = *next_pc;
    *next_pc = (trap_AfterBreakpoint << 16) | 0xd;
    os_flush_icache((os_vm_address_t) next_pc, sizeof(unsigned long));

#ifdef mach
    sigreturn(scp);
#endif
}

static void
sigtrap_handler(int signal, int code, struct sigcontext *scp)
{
    /* Don't disallow recursive breakpoint traps.  Otherwise, we can't */
    /* use debugger breakpoints anywhere in here. */
    sigsetmask(scp->sc_mask);

    switch (code) {
      case trap_PendingInterrupt:
	  arch_skip_instruction(scp);
	  interrupt_handle_pending(scp);
	  break;

      case trap_Halt:
	  fake_foreign_function_call(scp);
	  lose("%%primitive halt called; the party is over.\n");

      case trap_Error:
      case trap_Cerror:
	  interrupt_internal_error(signal, code, scp, code == trap_Cerror);
	  break;

      case trap_Breakpoint:
	  handle_breakpoint(signal, code, scp);
	  break;

      case trap_FunctionEndBreakpoint:
	  scp->sc_pc = (int) handle_function_end_breakpoint(signal, code, scp);
	  break;

      case trap_AfterBreakpoint:
	  *skipped_break_addr = (trap_Breakpoint << 16) | 0xd;
	  os_flush_icache((os_vm_address_t) skipped_break_addr,

			  sizeof(unsigned long));
	  skipped_break_addr = NULL;
	  *(unsigned long *) scp->sc_pc = displaced_after_inst;
	  os_flush_icache((os_vm_address_t) scp->sc_pc, sizeof(unsigned long));

	  scp->sc_mask = orig_sigmask;
	  break;

      default:
	  interrupt_handle_now(signal, code, scp);
	  break;
    }
}

#define FIXNUM_VALUE(lispobj) (((int)lispobj)>>2)

static void
sigfpe_handler(int signal, int code, struct sigcontext *scp)
{
    unsigned long bad_inst;
    unsigned int op, rs, rt, rd, funct, dest;
    int immed;
    long result;

    if (scp->sc_cause & CAUSE_BD)
	bad_inst = *(unsigned long *) (scp->sc_pc + 4);
    else
	bad_inst = *(unsigned long *) (scp->sc_pc);

    op = (bad_inst >> 26) & 0x3f;
    rs = (bad_inst >> 21) & 0x1f;
    rt = (bad_inst >> 16) & 0x1f;
    rd = (bad_inst >> 11) & 0x1f;
    funct = bad_inst & 0x3f;
    immed = (((int) (bad_inst & 0xffff)) << 16) >> 16;

    switch (op) {
      case 0x0:		/* SPECIAL */
	  switch (funct) {
	    case 0x20:		/* ADD */
		/* Check to see if this is really a pa_interrupted hit */
		if (rs == reg_ALLOC && rt == reg_NL4) {
		    scp->sc_regs[reg_ALLOC] +=
			(scp->sc_regs[reg_NL4] -
			 PSEUDO_ATOMIC_INTERRUPTED_BIAS);
		    arch_skip_instruction(scp);
		    interrupt_handle_pending(scp);
		    return;
		}
		result =
		    FIXNUM_VALUE(scp->sc_regs[rs]) +
		    FIXNUM_VALUE(scp->sc_regs[rt]);
		dest = rd;
		break;

	    case 0x22:		/* SUB */
		result =
		    FIXNUM_VALUE(scp->sc_regs[rs]) -
		    FIXNUM_VALUE(scp->sc_regs[rt]);
		dest = rd;
		break;

	    default:
		dest = 32;
		break;
	  }
	  break;

      case 0x8:		/* ADDI */
	  result = FIXNUM_VALUE(scp->sc_regs[rs]) + (immed >> 2);
	  dest = rt;
	  break;

      default:
	  dest = 32;
	  break;
    }

    if (dest < 32) {
	current_dynamic_space_free_pointer =
	    (lispobj *) scp->sc_regs[reg_ALLOC];

	scp->sc_regs[dest] = alloc_number(result);

	scp->sc_regs[reg_ALLOC] =
	    (unsigned long) current_dynamic_space_free_pointer;

	arch_skip_instruction(scp);

    } else
	interrupt_handle_now(signal, code, scp);
}

void
arch_install_interrupt_handlers()
{
    interrupt_install_low_level_handler(SIGTRAP, sigtrap_handler);
    interrupt_install_low_level_handler(SIGFPE, sigfpe_handler);
}

extern lispobj call_into_lisp(lispobj fun, lispobj * args, int nargs);

lispobj
funcall0(lispobj function)
{
    lispobj *args = current_control_stack_pointer;

    return call_into_lisp(function, args, 0);
}

lispobj
funcall1(lispobj function, lispobj arg0)
{
    lispobj *args = current_control_stack_pointer;

    current_control_stack_pointer += 1;
    args[0] = arg0;

    return call_into_lisp(function, args, 1);
}

lispobj
funcall2(lispobj function, lispobj arg0, lispobj arg1)
{
    lispobj *args = current_control_stack_pointer;

    current_control_stack_pointer += 2;
    args[0] = arg0;
    args[1] = arg1;

    return call_into_lisp(function, args, 2);
}

lispobj
funcall3(lispobj function, lispobj arg0, lispobj arg1, lispobj arg2)
{
    lispobj *args = current_control_stack_pointer;

    current_control_stack_pointer += 3;
    args[0] = arg0;
    args[1] = arg1;
    args[2] = arg2;

    return call_into_lisp(function, args, 3);
}


/* This is apparently called by emulate_branch, but isn't defined.  So */
/* just do nothing and hope it works... */

void
cacheflush(void)
{
}
