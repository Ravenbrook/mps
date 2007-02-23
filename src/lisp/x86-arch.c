/* x86-arch.c -*- Mode: C; comment-column: 40 -*-
 *
 * $Header: /project/cmucl/cvsroot/src/lisp/x86-arch.c,v 1.25 2005/10/06 21:52:53 rtoy Exp $ 
 *
 */

#include <stdio.h>

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

#define DPRINTF(test, e) {if(test) fprintf e ;}

#define BREAKPOINT_INST 0xcc	/* INT3 */

unsigned long fast_random_state = 1;

char *
arch_init(void)
{
    return "lisp.core";
}



/*
 * Assuming we get here via an INT3 xxx instruction, the PC now
 * points to the interrupt code (lisp value) so we just move past
 * it. Skip the code, then if the code is an error-trap or
 * Cerror-trap then skip the data bytes that follow.
 */

void
arch_skip_instruction(os_context_t * context)
{
    int vlen, code;

    DPRINTF(0, (stderr, "[arch_skip_inst at %lx>]\n", SC_PC(context)));

    /* Get and skip the lisp error code. */
    code = *(char *) SC_PC(context)++;
    switch (code) {
      case trap_Error:
      case trap_Cerror:
	  /* Lisp error arg vector length */
	  vlen = *(char *) SC_PC(context)++;
	  /* Skip lisp error arg data bytes */
	  while (vlen-- > 0)
	      SC_PC(context)++;
	  break;

      case trap_Breakpoint:
      case trap_FunctionEndBreakpoint:
	  break;

      case trap_PendingInterrupt:
      case trap_Halt:
	  /* Only needed to skip the Code. */
	  break;

      default:
	  fprintf(stderr, "[arch_skip_inst invalid code %d\n]\n", code);
	  break;
    }

    DPRINTF(0, (stderr, "[arch_skip_inst resuming at %lx>]\n", SC_PC(context)));
}

unsigned char *
arch_internal_error_arguments(os_context_t * context)
{
    return (unsigned char *) (SC_PC(context) + 1);
}

boolean
arch_pseudo_atomic_atomic(os_context_t * context)
{
    return SymbolValue(PSEUDO_ATOMIC_ATOMIC);
}

void
arch_set_pseudo_atomic_interrupted(os_context_t * context)
{
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, make_fixnum(1));
}



unsigned long
arch_install_breakpoint(void *pc)
{
    unsigned long result = *(unsigned long *) pc;

    *(char *) pc = BREAKPOINT_INST;	/* x86 INT3       */
    *((char *) pc + 1) = trap_Breakpoint;	/* Lisp trap code */

    return result;
}

void
arch_remove_breakpoint(void *pc, unsigned long orig_inst)
{
    *((char *) pc) = orig_inst & 0xff;
    *((char *) pc + 1) = (orig_inst & 0xff00) >> 8;
}



/*
 * When single stepping single_stepping holds the original instruction
 * pc location.
 */

unsigned int *single_stepping = NULL;

#ifndef __linux__
unsigned int single_step_save1;
unsigned int single_step_save2;
unsigned int single_step_save3;
#endif

void
arch_do_displaced_inst(os_context_t * context, unsigned long orig_inst)
{
    unsigned int *pc = (unsigned int *) SC_PC(context);

    /*
     * Put the original instruction back.
     */

    *((char *) pc) = orig_inst & 0xff;
    *((char *) pc + 1) = (orig_inst & 0xff00) >> 8;

#ifdef __linux__
    context->eflags |= 0x100;
#else

    /*
     * Install helper instructions for the single step:
     *    pushf; or [esp],0x100; popf.
     */

    single_step_save1 = *(pc - 3);
    single_step_save2 = *(pc - 2);
    single_step_save3 = *(pc - 1);
    *(pc - 3) = 0x9c909090;
    *(pc - 2) = 0x00240c81;
    *(pc - 1) = 0x9d000001;
#endif

    single_stepping = (unsigned int *) pc;

#ifndef __linux__
    (unsigned int *) SC_PC(context) = (char *) pc - 9;
#endif
}


void
sigtrap_handler(HANDLER_ARGS)
{
    unsigned int trap;

#ifdef __linux__
    GET_CONTEXT
#endif
#if 0
	fprintf(stderr, "x86sigtrap: %8x %x\n",
		SC_PC(context), *(unsigned char *) (SC_PC(context) - 1));
    fprintf(stderr, "sigtrap(%d %d %x)\n", signal, code, context);
#endif

    if (single_stepping && (signal == SIGTRAP)) {
#if 0
	fprintf(stderr, "* Single step trap %x\n", single_stepping);
#endif

#ifndef __linux__
	/* Un-install single step helper instructions. */
	*(single_stepping - 3) = single_step_save1;
	*(single_stepping - 2) = single_step_save2;
	*(single_stepping - 1) = single_step_save3;
#else
	context->eflags ^= 0x100;
#endif

	/*
	 * Re-install the breakpoint if possible.
	 */
	if ((int) SC_PC(context) == (int) single_stepping + 1)
	    fprintf(stderr, "* Breakpoint not re-install\n");
	else {
	    char *ptr = (char *) single_stepping;

	    ptr[0] = BREAKPOINT_INST;	/* x86 INT3 */
	    ptr[1] = trap_Breakpoint;
	}

	single_stepping = NULL;
	return;
    }

    SAVE_CONTEXT();

    /* This is just for info in case monitor wants to print an approx */
    current_control_stack_pointer = (unsigned long *) SC_SP(context);

#if defined(__linux__) && defined(i386)
    /*
     * Restore the FPU control word, setting the rounding mode to nearest.
     */

    if (contextstruct.fpstate)
	setfpucw(contextstruct.fpstate->cw & ~0xc00);
#endif

    /*
     * On entry %eip points just after the INT3 byte and aims at the
     * 'kind' value (eg trap_Cerror). For error-trap and Cerror-trap a
     * number of bytes will follow, the first is the length of the byte
     * arguments to follow.
     */

    trap = *(unsigned char *) SC_PC(context);

    switch (trap) {
      case trap_PendingInterrupt:
	  DPRINTF(0, (stderr, "<trap Pending Interrupt.>\n"));
	  arch_skip_instruction(context);
	  interrupt_handle_pending(context);
	  break;

      case trap_Halt:
	  {
#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
	      int fpu_state[27];

	      fpu_save(fpu_state);
#endif
	      fake_foreign_function_call(context);
	      lose("%%primitive halt called; the party is over.\n");
	      undo_fake_foreign_function_call(context);
#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
	      fpu_restore(fpu_state);
#endif
	      arch_skip_instruction(context);
	      break;
	  }

      case trap_Error:
      case trap_Cerror:
	  DPRINTF(0, (stderr, "<trap Error %d>\n", code));
#ifdef __linux__
	  interrupt_internal_error(signal, contextstruct, code == trap_Cerror);
#else
	  interrupt_internal_error(signal, code, context, code == trap_Cerror);
#endif
	  break;

      case trap_Breakpoint:
#if 0
	  fprintf(stderr, "*C break\n");
#endif
	  SC_PC(context) -= 1;

	  handle_breakpoint(signal, code, context);
#if 0
	  fprintf(stderr, "*C break return\n");
#endif
	  break;

      case trap_FunctionEndBreakpoint:
	  SC_PC(context) -= 1;
	  SC_PC(context) =
	      (int) handle_function_end_breakpoint(signal, code, context);
	  break;

#ifdef trap_DynamicSpaceOverflowWarning
      case trap_DynamicSpaceOverflowWarning:
	  interrupt_handle_space_overflow(SymbolFunction
					  (DYNAMIC_SPACE_OVERFLOW_WARNING_HIT),
					  context);
	  break;
#endif
#ifdef trap_DynamicSpaceOverflowError
      case trap_DynamicSpaceOverflowError:
	  interrupt_handle_space_overflow(SymbolFunction
					  (DYNAMIC_SPACE_OVERFLOW_ERROR_HIT),
					  context);
	  break;
#endif
      default:
	  DPRINTF(0,
		  (stderr, "[C--trap default %d %d %p]\n", signal, code,
		   context));
#ifdef __linux__
	  interrupt_handle_now(signal, contextstruct);
#else
	  interrupt_handle_now(signal, code, context);
#endif
	  break;
    }
}

#define FIXNUM_VALUE(lispobj) (((int) lispobj) >> 2)

void
arch_install_interrupt_handlers()
{
    interrupt_install_low_level_handler(SIGILL, sigtrap_handler);
    interrupt_install_low_level_handler(SIGTRAP, sigtrap_handler);
}


extern lispobj call_into_lisp(lispobj fun, lispobj * args, int nargs);

/* These next four functions are an interface to the 
 * Lisp call-in facility. Since this is C we can know
 * nothing about the calling environment. The control
 * stack might be the C stack if called from the monitor
 * or the Lisp stack if called as a result of an interrupt
 * or maybe even a separate stack. The args are most likely
 * on that stack but could be in registers depending on
 * what the compiler likes. So I try to package up the
 * args into a portable vector and let the assembly language
 * call-in function figure it out.
 */

lispobj
funcall0(lispobj function)
{
    lispobj *args = NULL;

    return call_into_lisp(function, args, 0);
}

lispobj
funcall1(lispobj function, lispobj arg0)
{
    lispobj args[1];

    args[0] = arg0;
    return call_into_lisp(function, args, 1);
}

lispobj
funcall2(lispobj function, lispobj arg0, lispobj arg1)
{
    lispobj args[2];

    args[0] = arg0;
    args[1] = arg1;
    return call_into_lisp(function, args, 2);
}

lispobj
funcall3(lispobj function, lispobj arg0, lispobj arg1, lispobj arg2)
{
    lispobj args[3];

    args[0] = arg0;
    args[1] = arg1;
    args[2] = arg2;
    return call_into_lisp(function, args, 3);
}

#ifdef LINKAGE_TABLE

#ifndef LinkageEntrySize
#define LinkageEntrySize 8
#endif

void
arch_make_linkage_entry(long linkage_entry, void *target_addr, long type)
{
    char *reloc_addr = (char *) (FOREIGN_LINKAGE_SPACE_START

				 + linkage_entry * LinkageEntrySize);

    if (type == 1) {		/* code reference */
	/* Make JMP to function entry. */
	/* JMP offset is calculated from next instruction. */
	long offset = (char *) target_addr - (reloc_addr + 5);
	int i;

	*reloc_addr++ = 0xe9;	/* opcode for JMP rel32 */
	for (i = 0; i < 4; i++) {
	    *reloc_addr++ = offset & 0xff;
	    offset >>= 8;
	}
	/* write a nop for good measure. */
	*reloc_addr = 0x90;
    } else if (type == 2) {
	*(unsigned long *) reloc_addr = (unsigned long) target_addr;
    }
}

/* Make a call to the first function in the linkage table, which is
   resolve_linkage_tramp. */
void
arch_make_lazy_linkage(long linkage_entry)
{
    char *reloc_addr = (char *) (FOREIGN_LINKAGE_SPACE_START

				 + linkage_entry * LinkageEntrySize);
    long offset = (char *) (FOREIGN_LINKAGE_SPACE_START) - (reloc_addr + 5);
    int i;

    *reloc_addr++ = 0xe8;	/* opcode for CALL rel32 */
    for (i = 0; i < 4; i++) {
	*reloc_addr++ = offset & 0xff;
	offset >>= 8;
    }
    /* write a nop for good measure. */
    *reloc_addr = 0x90;
}

/* Get linkage entry.  The initial instruction in the linkage
   entry is a CALL; the return address we're passed points to the next
   instruction. */

long
arch_linkage_entry(unsigned long retaddr)
{
    return ((retaddr - 5) - FOREIGN_LINKAGE_SPACE_START) / LinkageEntrySize;
}
#endif /* LINKAGE_TABLE */
