/*

 $Header: /project/cmucl/cvsroot/src/lisp/hppa-arch.c,v 1.10 2005/09/15 18:26:51 rtoy Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>
#include <machine/trap.h>

#include "lisp.h"
#include "globals.h"
#include "validate.h"
#include "os.h"
#include "arch.h"
#include "lispregs.h"
#include "signal.h"
#include "internals.h"
#include "breakpoint.h"

char *
arch_init(void)
{
    return NULL;
}

os_vm_address_t
arch_get_bad_addr(int signal, int code, struct sigcontext * scp)
{
#ifdef hpux
    struct save_state *state;
    os_vm_address_t addr;

    state = (struct save_state *) (&(scp->sc_sl.sl_ss));

    if (state == NULL)
	return NULL;

    /* Check the instruction address first. */
    addr = (os_vm_address_t) ((unsigned long) scp->sc_pcoq_head & ~3);
    if (addr < (os_vm_address_t) 0x1000)
	return addr;

    /* Otherwise, it must have been a data fault. */
    return (os_vm_address_t) state->ss_cr21;
#else
    struct hp800_thread_state *state;
    os_vm_address_t addr;

    state = (struct hp800_thread_state *) (scp->sc_ap);

    if (state == NULL)
	return NULL;

    /* Check the instruction address first. */
    addr = scp->sc_pcoqh & ~3;
    if (addr < 0x1000)
	return addr;

    /* Otherwise, it must have been a data fault. */
    return state->cr21;
#endif
}

unsigned char *
arch_internal_error_arguments(struct sigcontext *scp)
{
#ifdef hpux
    return (unsigned char *) ((scp->sc_pcoq_head & ~0x3) + 4);
#else
    return (unsigned char *) ((scp->sc_pcoqh & ~0x3) + 4);
#endif
}

boolean
arch_pseudo_atomic_atomic(struct sigcontext *scp)
{
    /* Pseudo-atomic-atomic is implemented by oring 0x4 into ALLOC. */

    if (SC_REG(scp, reg_ALLOC) & 0x4)
	return TRUE;
    else
	return FALSE;
}

void
arch_set_pseudo_atomic_interrupted(struct sigcontext *scp)
{
    /* Pseudo-atomic-atomic is implemented by oring 0x1 into ALLOC. */

    SC_REG(scp, reg_ALLOC) |= 1;
}

void
arch_skip_instruction(struct sigcontext *scp)
{
    /* Skip the offending instruction */
#ifdef hpux
    scp->sc_pcoq_head = scp->sc_pcoq_tail;
    scp->sc_pcoq_tail += 4;
#else
    scp->sc_pcoqh = scp->sc_pcoqt;
    scp->sc_pcoqt += 4;
#endif
}

unsigned long
arch_install_breakpoint(void *pc)
{
    unsigned long *ulpc = (unsigned long *) pc;
    unsigned long orig_inst = *ulpc;

    *ulpc = trap_Breakpoint;
    os_flush_icache((os_vm_address_t) pc, sizeof(*ulpc));
    return orig_inst;
}

void
arch_remove_breakpoint(void *pc, unsigned long orig_inst)
{
    unsigned long *ulpc = (unsigned long *) pc;

    *ulpc = orig_inst;
    os_flush_icache((os_vm_address_t) pc, sizeof(*ulpc));
}

#ifdef hpux
extern void SingleStepTraps();
static unsigned long *BreakpointAddr = NULL;
static unsigned long NextPc = NULL;
#endif

void
arch_do_displaced_inst(struct sigcontext *scp, unsigned long orig_inst)
{
#ifdef hpux
    /* We change the next-pc to point to a breakpoint instruction, restore */
    /* the original instruction, and exit.  We would like to be able to */
    /* sigreturn, but we can't, because this is hpux. */
    unsigned long *pc = (unsigned long *) (SC_PC(scp) & ~3);

    NextPc = SC_NPC(scp);
    SC_NPC(scp) = (unsigned) SingleStepTraps | (SC_NPC(scp) & 3);

    BreakpointAddr = pc;
    *pc = orig_inst;
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned long));
#else
    /* We set the recovery counter to cover one instruction, put the */
    /* original instruction back in, and then resume.  We will then trap */
    /* after executing that one instruction, at which time we can put */
    /* the breakpoint back in. */

    ((struct hp800_thread_state *) scp->sc_ap)->cr0 = 1;
    scp->sc_ps |= 0x10;
    *(unsigned long *) SC_PC(scp) = orig_inst;

    sigreturn(scp);
#endif
}

#ifdef hpux
static void
restore_breakpoint(struct sigcontext *scp)
{
    /* We just single-stepped over an instruction that we want to replace */
    /* with a breakpoint.  So we put the breakpoint back in, and tweek the */
    /* state so that we will continue as if nothing happened. */

    if (NextPc == NULL)
	lose("SingleStepBreakpoint trap at strange time.");

    if ((SC_PC(scp) & ~3) == (unsigned long) SingleStepTraps) {
	/* The next instruction was not nullified. */
	SC_PC(scp) = NextPc;
	if ((SC_NPC(scp) & ~3) == (unsigned long) SingleStepTraps + 4) {
	    /* The instruction we just stepped over was not a branch, so */
	    /* we need to fix it up.  If it was a branch, it will point to */
	    /* the correct place. */
	    SC_NPC(scp) = NextPc + 4;
	}
    } else {
	/* The next instruction was nullified, so we want to skip it. */
	SC_PC(scp) = NextPc + 4;
	SC_NPC(scp) = NextPc + 8;
    }
    NextPc = NULL;

    if (BreakpointAddr) {
	*BreakpointAddr = trap_Breakpoint;
	os_flush_icache((os_vm_address_t) BreakpointAddr,

			sizeof(unsigned long));
	BreakpointAddr = NULL;
    }
}
#endif

static void
sigtrap_handler(int signal, int code, struct sigcontext *scp)
{
    unsigned long bad_inst;

    sigsetmask(scp->sc_mask);

#if 0
    printf("sigtrap_handler, pc=0x%08x, alloc=0x%08x\n", scp->sc_pcoqh,
	   SC_REG(scp, reg_ALLOC));
#endif

#ifdef hpux
    bad_inst = *(unsigned long *) (scp->sc_pcoq_head & ~3);
#else
    bad_inst = *(unsigned long *) (scp->sc_pcoqh & ~3);
#endif

    if (bad_inst & 0xfc001fe0)
	interrupt_handle_now(signal, code, scp);
    else {
	int im5 = bad_inst & 0x1f;

	switch (im5) {
	  case trap_Halt:
	      fake_foreign_function_call(scp);
	      lose("%%primitive halt called; the party is over.\n");

	  case trap_PendingInterrupt:
	      arch_skip_instruction(scp);
	      interrupt_handle_pending(scp);
	      break;

	  case trap_Error:
	  case trap_Cerror:
	      interrupt_internal_error(signal, code, scp, im5 == trap_Cerror);
	      break;

	  case trap_Breakpoint:
	      sigsetmask(scp->sc_mask);
	      handle_breakpoint(signal, code, scp);
	      break;

	  case trap_FunctionEndBreakpoint:
	      sigsetmask(scp->sc_mask);
	      {
		  unsigned long pc;

		  pc = (unsigned long)
		      handle_function_end_breakpoint(signal, code, scp);
#ifdef hpux
		  scp->sc_pcoq_head = pc;
		  scp->sc_pcoq_tail = pc + 4;
#else
		  scp->sc_pcoqh = pc;
		  scp->sc_pcoqt = pc + 4;
#endif
	      }
	      break;

	  case trap_SingleStepBreakpoint:
	      restore_breakpoint(scp);
	      break;

	  default:
	      interrupt_handle_now(signal, code, scp);
	      break;
	}
    }
}

static void
sigfpe_handler(int signal, int code, struct sigcontext *scp)
{
    unsigned long badinst;
    int opcode, r1, r2, t;
    long op1, op2, res;

#if 0
    printf("sigfpe_handler, pc=0x%08x, alloc=0x%08x\n", scp->sc_pcoqh,
	   SC_REG(scp, reg_ALLOC));
#endif

    switch (code) {
      case I_OVFLO:
	  badinst = *(unsigned long *) (SC_PC(scp) & ~3);
	  opcode = badinst >> 26;

	  if (opcode == 2) {
	      /* reg/reg inst. */
	      r1 = (badinst >> 16) & 0x1f;
	      op1 = fixnum_value(SC_REG(scp, r1));
	      r2 = (badinst >> 21) & 0x1f;
	      op2 = fixnum_value(SC_REG(scp, r2));
	      t = badinst & 0x1f;

	      switch ((badinst >> 5) & 0x7f) {
		case 0x70:
		    /* Add and trap on overflow. */
		    res = op1 + op2;
		    break;

		case 0x60:
		    /* Subtract and trap on overflow. */
		    res = op1 - op2;
		    break;

		default:
		    goto not_interesting;
	      }
	  } else if ((opcode & 0x37) == 0x25 && (badinst & (1 << 11))) {
	      /* Add or subtract immediate. */
	      op1 = ((badinst >> 3) & 0xff) | ((-badinst & 1) << 8);
	      r2 = (badinst >> 16) & 0x1f;
	      op2 = fixnum_value(SC_REG(scp, r1));
	      t = (badinst >> 21) & 0x1f;
	      if (opcode == 0x2d)
		  res = op1 + op2;
	      else
		  res = op1 - op2;
	  } else
	      goto not_interesting;

	  current_dynamic_space_free_pointer =
	      (lispobj *) SC_REG(scp, reg_ALLOC);
	  SC_REG(scp, t) = alloc_number(res);
	  SC_REG(scp, reg_ALLOC)
	      = (unsigned long) current_dynamic_space_free_pointer;
	  arch_skip_instruction(scp);

	  break;

      case I_COND:
	  badinst = *(unsigned long *) (SC_PC(scp) & ~3);
	  if ((badinst & 0xfffff800) ==
	      (0xb000e000 | reg_ALLOC << 21 | reg_ALLOC << 16)) {
	      /* It is an ADDIT,OD i,ALLOC,ALLOC instruction that trapped. */
	      /* That means that it is the end of a pseudo-atomic.  So do the */
	      /* add stripping off the pseudo-atomic-interrupted bit, and then */
	      /* tell the machine-independent code to process the pseudo- */
	      /* atomic. */
	      int immed = (badinst >> 1) & 0x3ff;

	      if (badinst & 1)
		  immed |= -1 << 10;
	      SC_REG(scp, reg_ALLOC) += (immed - 1);
	      arch_skip_instruction(scp);
	      interrupt_handle_pending(scp);
	      break;
	  }
	  /* else drop-through. */
      default:
	not_interesting:
	  interrupt_handle_now(signal, code, scp);
    }
}

void
arch_install_interrupt_handlers(void)
{
#ifdef hpux
    interrupt_install_low_level_handler(SIGILL, sigtrap_handler);
#endif
    interrupt_install_low_level_handler(SIGTRAP, sigtrap_handler);
    interrupt_install_low_level_handler(SIGFPE, sigfpe_handler);
}

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
