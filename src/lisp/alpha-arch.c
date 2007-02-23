/*

 $Header: /project/cmucl/cvsroot/src/lisp/alpha-arch.c,v 1.8 2005/09/15 18:26:50 rtoy Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>
#include <string.h>

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

extern char call_into_lisp_LRA[], call_into_lisp_end[];

#define BREAKPOINT_INST 0

char *
arch_init(void)
{
    if (mmap((os_vm_address_t) call_into_lisp_LRA_page, OS_VM_DEFAULT_PAGESIZE,
	     OS_VM_PROT_ALL, MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, -1, 0)
	== (os_vm_address_t) - 1)
	perror("mmap");
    memcpy(call_into_lisp_LRA_page, call_into_lisp_LRA, OS_VM_DEFAULT_PAGESIZE);
    os_flush_icache((os_vm_address_t) call_into_lisp_LRA_page,
		    OS_VM_DEFAULT_PAGESIZE);
    return NULL;
}

os_vm_address_t
arch_get_bad_addr(int sig, int code, struct sigcontext * scp)
{
    unsigned int badinst;

    if ((scp->sc_pc & 3) != 0)
	return NULL;

    if ((scp->sc_pc < READ_ONLY_SPACE_START ||
	 scp->sc_pc >= READ_ONLY_SPACE_START + READ_ONLY_SPACE_SIZE) &&
	((lispobj *) scp->sc_pc < current_dynamic_space ||
	 (lispobj *) scp->sc_pc >= current_dynamic_space + dynamic_space_size))
	return NULL;

    badinst = *(unsigned int *) scp->sc_pc;

    if (((badinst >> 27) != 0x16)	/* STL or STQ */
	&&((badinst >> 27) != 0x13))	/* STS or STT */
	return NULL;		/* Otherwise forget about address */

    return (os_vm_address_t) (scp->sc_regs[(badinst >> 16) & 0x1f] +
			      (badinst & 0xffff));
}

void
arch_skip_instruction(scp)
     struct sigcontext *scp;
{
    scp->sc_pc = +4;
}

unsigned char *
arch_internal_error_arguments(struct sigcontext *scp)
{
    return (unsigned char *) (scp->sc_pc + 4);
}

boolean
arch_pseudo_atomic_atomic(struct sigcontext *scp)
{
    return (scp->sc_regs[reg_ALLOC] & 1);
}

void
arch_set_pseudo_atomic_interrupted(struct sigcontext *scp)
{
#ifdef __linux__
    scp->sc_regs[reg_ALLOC] |= (1 << 63);
#else
    scp->sc_regs[reg_ALLOC] |= 2;
#endif
}

unsigned long
arch_install_breakpoint(void *pc)
{
    unsigned int *ptr = (unsigned int *) pc;
    unsigned long result = (unsigned long) *ptr;

    *ptr = BREAKPOINT_INST;

    os_flush_icache((os_vm_address_t) ptr, sizeof(unsigned long));

    return result;
}

void
arch_remove_breakpoint(void *pc, unsigned long orig_inst)
{
    unsigned int *ptr = (unsigned int) pc;

    *ptr = orig_inst;
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned long));
}

static unsigned int *skipped_break_addr, displaced_after_inst, after_breakpoint;

#ifdef POSIX_SIGS
static sigset_t orig_sigmask;
#else
static int orig_sigmask;
#endif

unsigned int
emulate_branch(struct sigcontext *scp, unsigned long orig_inst)
{
    int op = orig_inst >> 26;
    int reg_a = (orig_inst >> 21) & 0x1f;
    int reg_b = (orig_inst >> 16) & 0x1f;
    int fn = orig_inst & 0xffff;
    int disp =

	(orig_inst & (1 << 20)) ? orig_inst | (-1 << 21) : orig_inst & 0x1fffff;
    int next_pc = scp->sc_pc;
    int branch = NULL;

    switch (op) {
      case 0x1a:		/* jmp, jsr, jsr_coroutine, ret */
	  scp->sc_regs[reg_a] = scp->sc_pc;
	  scp->sc_pc = scp->sc_regs[reg_b] & ~3;
	  break;
      case 0x30:		/* br */
	  scp->sc_regs[reg_a] = scp->sc_pc;
	  branch = 1;
	  break;
      case 0x31:		/* fbeq */
	  if (scp->sc_fpregs[reg_a] == 0)
	      branch = 1;
	  break;
      case 0x32:		/* fblt */
	  if (scp->sc_fpregs[reg_a] < 0)
	      branch = 1;
	  break;
      case 0x33:		/* fble */
	  if (scp->sc_fpregs[reg_a] <= 0)
	      branch = 1;
	  break;
      case 0x34:		/* bsr */
	  scp->sc_regs[reg_a] = scp->sc_pc;
	  branch = 1;
	  break;
      case 0x35:		/* fbne */
	  if (scp->sc_regs[reg_a] != 0)
	      branch = 1;
	  break;
      case 0x36:		/* fbge */
	  if (scp->sc_fpregs[reg_a] >= 0)
	      branch = 1;
	  break;
      case 0x37:		/* fbgt */
	  if (scp->sc_fpregs[reg_a] > 0)
	      branch = 1;
	  break;
      case 0x38:		/* blbc */
	  if ((scp->sc_regs[reg_a] & 1) == 0)
	      branch = 1;
	  break;
      case 0x39:		/* beq */
	  if (scp->sc_regs[reg_a] == 0)
	      branch = 1;
	  break;
      case 0x3a:		/* blt */
	  if (scp->sc_regs[reg_a] < 0)
	      branch = 1;
	  break;
      case 0x3b:		/* ble */
	  if (scp->sc_regs[reg_a] <= 0)
	      branch = 1;
	  break;
      case 0x3c:		/* blbs */
	  if ((scp->sc_regs[reg_a] & 1) != 0)
	      branch = 1;
	  break;
      case 0x3d:		/* bne */
	  if (scp->sc_regs[reg_a] != 0)
	      branch = 1;
	  break;
      case 0x3e:		/* bge */
	  if (scp->sc_regs[reg_a] >= 0)
	      branch = 1;
	  break;
      case 0x3f:		/* bgt */
	  if (scp->sc_regs[reg_a] > 0)
	      branch = 1;
	  break;
    }
    if (branch)
	next_pc += disp * 4;
    return next_pc;
}

void
arch_do_displaced_inst(struct sigcontext *scp, unsigned long orig_inst)
{
    unsigned int *pc = scp->sc_pc;
    unsigned int *next_pc;
    unsigned int next_inst;
    int op = orig_inst >> 26;;

#ifdef POSIX_SIGS
#if !defined(__linux__) || (defined(__linux__) && (__GNU_LIBRARY__ < 6))
    orig_sigmask = context->uc_sigmask;
    FILLBLOCKSET(&context->uc_sigmask);
#else
    {
	sigset_t temp;

	sigemptyset(&temp);
	orig_sigmask.__val[0] = scp->uc_sigmask;
	temp.__val[0] = scp->uc_sigmask;
	FILLBLOCKSET(&temp);

	scp->uc_sigmask = temp.__val[0];
    }
#endif
#else
    orig_sigmask = scp->sc_mask;
    scp->sc_mask = BLOCKABLE;
#endif

    /* Figure out where the displaced inst is going */
    if (op == 0x1a || op & 0xf == 0x30)	/* branch...ugh */
	next_pc = (unsigned int *) emulate_branch(scp, orig_inst);
    else
	next_pc = pc + 1;

    /* Put the original instruction back. */
    *pc = orig_inst;
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned long));

    skipped_break_addr = pc;

    /* set the after breakpoint */
    displaced_after_inst = *next_pc;
    *next_pc = BREAKPOINT_INST;
    after_breakpoint = 1;
    os_flush_icache((os_vm_address_t) next_pc, sizeof(unsigned long));

    sigreturn(scp);
}

#define AfterBreakpoint 100

static void
sigtrap_handler(int signal, int code, struct sigcontext *scp)
{
    /* Don't disallow recursive breakpoint traps.  Otherwise, we can't */
    /* use debugger breakpoints anywhere in here. */
    sigsetmask(scp->sc_mask);

    if (*(unsigned int *) (scp->sc_pc - 4) == BREAKPOINT_INST) {
	if (after_breakpoint)
	    code = AfterBreakpoint;
	else
	    code = trap_Breakpoint;
    } else
	code = *(u32 *) scp->sc_pc;

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
	  scp->sc_pc -= 4;
	  handle_breakpoint(signal, code, scp);
	  break;

      case trap_FunctionEndBreakpoint:
	  scp->sc_pc -= 4;
	  scp->sc_pc = (int) handle_function_end_breakpoint(signal, code, scp);
	  break;

      case AfterBreakpoint:
	  scp->sc_pc -= 4;
	  *skipped_break_addr = BREAKPOINT_INST;
	  os_flush_icache((os_vm_address_t) skipped_break_addr,

			  sizeof(unsigned long));
	  skipped_break_addr = NULL;
	  *(unsigned int *) scp->sc_pc = displaced_after_inst;
	  os_flush_icache((os_vm_address_t) scp->sc_pc, sizeof(unsigned long));

#ifdef POSIX_SIGS
#if  !defined(__linux__) || (defined(__linux__) && (__GNU_LIBRARY__ < 6))
	  scp->sc_mask = orig_sigmask;
#else
	  scp->sc_mask = orig_sigmask.__val[0];
#endif
#else
	  scp->sc_mask = orig_sigmask;
#endif
	  after_breakpoint = NULL;
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
}

void
arch_install_interrupt_handlers()
{
    interrupt_install_low_level_handler(SIGILL, sigtrap_handler);
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
