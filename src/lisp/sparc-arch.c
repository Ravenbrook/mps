/*

 $Header: /project/cmucl/cvsroot/src/lisp/sparc-arch.c,v 1.27 2005/09/15 18:26:52 rtoy Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>
#ifdef SOLARIS
#include <sys/trap.h>
#else
#include <machine/trap.h>
#endif

#include "arch.h"
#include "lisp.h"
#include "internals.h"
#include "globals.h"
#include "validate.h"
#include "os.h"
#include "lispregs.h"
#include "signal.h"
#include "interrupt.h"
#include "gencgc.h"
#include "breakpoint.h"
#include "interr.h"

char *
arch_init()
{
    return 0;
}

os_vm_address_t
arch_get_bad_addr(HANDLER_ARGS)
{
    unsigned int badinst;
    int rs1;

    /* On the sparc, we have to decode the instruction. */

    /* Make sure it's not the pc thats bogus, and that it was lisp code */
    /* that caused the fault. */
    if ((SC_PC(context) & 3) != 0 ||
	((SC_PC(context) < READ_ONLY_SPACE_START ||
	  SC_PC(context) >= READ_ONLY_SPACE_START + READ_ONLY_SPACE_SIZE) &&
	 ((lispobj *) SC_PC(context) < current_dynamic_space &&
	  (lispobj *) SC_PC(context) >=
	  current_dynamic_space + dynamic_space_size))) return 0;

    badinst = *(unsigned int *) SC_PC(context);

    if ((badinst >> 30) != 3)
	/* All load/store instructions have op = 11 (binary) */
	return 0;

    rs1 = (badinst >> 14) & 0x1f;

    if (badinst & (1 << 13)) {
	/* r[rs1] + simm(13) */
	int simm13 = badinst & 0x1fff;

	if (simm13 & (1 << 12))
	    simm13 |= -1 << 13;

	return (os_vm_address_t) (SC_REG(context, rs1) + simm13);
    } else {
	/* r[rs1] + r[rs2] */
	int rs2 = badinst & 0x1f;

	return (os_vm_address_t) (SC_REG(context, rs1) + SC_REG(context, rs2));
    }

}

void
arch_skip_instruction(context)
     struct sigcontext *context;
{
    /* Skip the offending instruction */
    SC_PC(context) = SC_NPC(context);
    SC_NPC(context) += 4;
}

unsigned char *
arch_internal_error_arguments(struct sigcontext *scp)
{
    return (unsigned char *) (SC_PC(scp) + 4);
}

boolean
arch_pseudo_atomic_atomic(struct sigcontext *scp)
{
    return (SC_REG(scp, reg_ALLOC) & pseudo_atomic_Value);
}

void
arch_set_pseudo_atomic_interrupted(struct sigcontext *scp)
{
    SC_REG(scp, reg_ALLOC) |= pseudo_atomic_InterruptedValue;
}

unsigned long
arch_install_breakpoint(void *pc)
{
    unsigned int *ptr = (unsigned int *) pc;
    unsigned int result = *ptr;

    *ptr = trap_Breakpoint;
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned int));

    return result;
}

void
arch_remove_breakpoint(void *pc, unsigned long orig_inst)
{
    *(unsigned int *) pc = (unsigned int) orig_inst;
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned int));
}

static unsigned int *skipped_break_addr, displaced_after_inst;

#ifdef POSIX_SIGS
static sigset_t orig_sigmask;
#else
static int orig_sigmask;
#endif

void
arch_do_displaced_inst(struct sigcontext *scp, unsigned long orig_inst)
{
    unsigned int *pc = (unsigned int *) SC_PC(scp);
    unsigned int *npc = (unsigned int *) SC_NPC(scp);

#ifdef POSIX_SIGS
    orig_sigmask = scp->uc_sigmask;
    sigemptyset(&scp->uc_sigmask);
    FILLBLOCKSET(&scp->uc_sigmask);
#else
    orig_sigmask = scp->sc_mask;
    scp->sc_mask = BLOCKABLE;
#endif

    *pc = orig_inst;
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned int));

    skipped_break_addr = pc;
    displaced_after_inst = *npc;
    *npc = trap_AfterBreakpoint;
    os_flush_icache((os_vm_address_t) npc, sizeof(unsigned int));

#ifdef SOLARIS
    /* XXX never tested */
    setcontext(scp);
#else
    sigreturn(scp);
#endif
}

/*
 * Look at the instruction at address PC and see if it's a trap
 * instruction with an immediate value.  If so, set trapno to the trap
 * number, and return non-zero.  If it's not a trap instruction,
 * return 0.
 */
boolean
trap_inst_p(unsigned int *pc, int *trapno)
{
    unsigned int trap_inst;

    trap_inst = *pc;

    if (((trap_inst >> 30) == 2)
	&& (((trap_inst >> 19) & 0x3f) == 0x3a)
	&& (((trap_inst >> 14) & 0x1f) == reg_ZERO)
	&& (((trap_inst >> 13) & 1) == 1)) {
	/*
	 * Got a trap instruction with immediate trap value.
	 * Get the value and return.
	 */
	*trapno = (trap_inst & 0x3f);

	return 1;
    } else {
	*trapno = -1;
	return 0;
    }
}


static int
pseudo_atomic_trap_p(struct sigcontext *context)
{
    unsigned int *pc;
    unsigned int badinst;
    int trapno;
    int result;


    pc = (unsigned int *) SC_PC(context);
    badinst = *pc;
    result = 0;

    /*
     * Check to see if the current instruction is a trap #16.  We check
     * to make sure this instruction was a trap instruction with rs1 = 0
     * and a software trap number (immediate value) of 16.
     */
    if (trap_inst_p(pc, &trapno) && (trapno == trap_PseudoAtomic)) {
	unsigned int previnst;

	previnst = pc[-1];
	/*
	 * Check to see if the previous instruction was an andcc alloc-tn,
	 * pseudo_atomic_InterruptedValue, zero-tn instruction.
	 */
	if (((previnst >> 30) == 2) && (((previnst >> 19) & 0x3f) == 0x11)
	    && (((previnst >> 14) & 0x1f) == reg_ALLOC)
	    && (((previnst >> 25) & 0x1f) == reg_ZERO)
	    && (((previnst >> 13) & 1) == 1)
	    && ((previnst & 0x1fff) == pseudo_atomic_InterruptedValue)) {
	    result = 1;
	} else {
	    fprintf(stderr,
		    "Oops!  Got a pseudo atomic trap without a preceeding andcc!\n");
	}
    }
    return result;
}

#ifdef GENCGC
/*
 * Return non-zero if the instruction is a trap 31 instruction
 */

boolean
allocation_trap_p(struct sigcontext * context)
{
    int result;
    unsigned int *pc;
    unsigned int or_inst;
    int trapno;

    result = 0;

    /*
     * Make sure this is a trap 31 instruction preceeded by an OR
     * instruction.
     */

    pc = (unsigned int *) SC_PC(context);

    if (trap_inst_p(pc, &trapno) && (trapno == trap_Allocation)) {
	/* Got the trap.  Is it preceeded by an OR instruction or SUB
	   instruction? */
	or_inst = pc[-1];
	if ((((or_inst >> 30) == 2) && (((or_inst >> 19) & 0x1f) == 2)) ||
	    (((or_inst >> 30) == 2) && (((or_inst >> 19) & 0x1f) == 4))) {
	    result = 1;
	} else {
	    fprintf(stderr,
		    "Whoa!!! Got an allocation trap not preceeded by an OR inst: 0x%08x!\n",
		    or_inst);
	}
    }

    return result;
}
#endif

#if 0
/* Pop the stack frame that build_fake_control_stack_frame makes */
static void
pop_fake_control_stack_frame(struct sigcontext *context)
{
    current_control_frame_pointer = (lispobj *) SC_REG(context, reg_CFP);
    SC_REG(context, reg_OCFP) = current_control_frame_pointer[0];
    SC_REG(context, reg_CODE) = current_control_frame_pointer[1];
    SC_REG(context, reg_CSP) = SC_REG(context, reg_CFP);
    SC_REG(context, reg_CFP) = SC_REG(context, reg_OCFP);
}
#endif

/*
 * Use this function to enable the minimum number of signals we need
 * when our trap handler needs to call Lisp code that might cons.  For
 * consing to work with gencgc, we need to be able to trap the SIGILL
 * signal to perform allocation.
 */
void
enable_some_signals()
{
#ifdef GENCGC
    sigset_t sigs;

    sigemptyset(&sigs);
    sigaddset(&sigs, SIGILL);
    sigprocmask(SIG_UNBLOCK, &sigs, NULL);
#endif
}

#ifdef GENCGC
void
handle_allocation_trap(struct sigcontext *context)
{
    unsigned int *pc;
    unsigned int or_inst;
    int target;
    int size;
    int immed;
    boolean were_in_lisp;
    char *memory;
    sigset_t block;

    target = 0;
    size = 0;

#if 0
    /*
     * Block all blockable signals.  Need to do this because
     * sigill_handler enables the signals.  When the handler returns,
     * signals should be enabled again, automatically.
     */

    sigemptyset(&block);
    FILLBLOCKSET(&block);
    sigprocmask(SIG_BLOCK, &block, 0);
#else
    /*
     * Well, maybe not.  sigill_handler probably shouldn't be unblocking
     * all signals.  So, let's enable just the signals we need.  Since
     * alloc might call GC, we need to have SIGILL enabled so we can do
     * allocation.  Do we need more?
     */
    enable_some_signals();
#endif

    pc = (unsigned int *) SC_PC(context);
    or_inst = pc[-1];

    /*
     * The instruction before this trap instruction had better be an OR
     * instruction or SUB instruction!
     */

    if (((or_inst >> 30) == 2) && (((or_inst >> 19) & 0x1f) == 2)) {
	/*
	 * An OR instruction.  RS1 is the register we want to allocate
	 * to.  RS2 (or an immediate) is the size.
	 */

	target = (or_inst >> 14) & 0x1f;

	immed = (or_inst >> 13) & 1;

	if (immed == 1) {
	    size = or_inst & 0x1fff;
	} else {
	    size = or_inst & 0x1f;
	    size = SC_REG(context, size);
	}
    } else if (((or_inst >> 30) == 2) && (((or_inst >> 19) & 0x1f) == 4)) {
	/*
	 * A SUB instruction.  RD is the register to allocate to, RS2
	 * (or an immediate) is the size.
	 */

	target = (or_inst >> 25) & 0x1f;
	immed = (or_inst >> 13) & 1;
	if (immed == 1) {
	    size = or_inst & 0x1fff;
	} else {
	    size = or_inst & 0x1f;
	    size = SC_REG(context, size);
	}
    }


    /*
     * I don't think it's possible for us NOT to be in lisp when we get
     * here.  Remove this later?
     */
    were_in_lisp = !foreign_function_call_active;

    if (were_in_lisp) {
	fake_foreign_function_call(context);
    } else {
	fprintf(stderr, "**** Whoa! allocation trap and we weren't in lisp!\n");
    }

    /*
     * alloc-tn was incremented by size.  If we get here, we need to
     * decrement it by size to restore it's original value.
     */
    /*  current_dynamic_space_free_pointer = (lispobj *) SC_REG(context, reg_ALLOC); */
    current_dynamic_space_free_pointer =
	(lispobj *) ((long) current_dynamic_space_free_pointer - size);

    /*
     * Allocate some memory, store the memory address in target.
     */

#if 0
    fprintf(stderr, "Alloc %d to %s\n", size, lisp_register_names[target]);
#endif

    memory = (char *) alloc(size);
    SC_REG(context, target) = (unsigned long) memory;
    SC_REG(context, reg_ALLOC) =
	(unsigned long) current_dynamic_space_free_pointer;

    if (were_in_lisp) {
	undo_fake_foreign_function_call(context);
    }

}
#endif

/*
 * How to identify an illegal instruction trap and a trap instruction
 * trap.
 */
#ifdef SOLARIS
#define ILLTRAP_INST ILL_ILLOPC
#define TRAP_INST(code) (CODE(code) == ILL_ILLTRP)
#else
#define ILLTRAP_INST T_UNIMP_INSTR
#define TRAP_INST(code) ((CODE(code) >= T_SOFTWARE_TRAP + 16) && (CODE(code) < T_SOFTWARE_TRAP + 32))
#endif

static void
sigill_handler(HANDLER_ARGS)
{
    SAVE_CONTEXT();

#ifdef POSIX_SIGS
    /*
     * Do we really want to have the same signals as in the context?
     * This would typically enable all signals, I think.  But there
     * are comments in interrupt_handle_now that says we want to
     * alloc_sap while interrupts are disabled.  The interrupt
     * handlers that eventually get called from here will re-enable
     * interrupts at the appropriate time, so we don't do anything
     * here.
     *
     * (I'm guessing here.  I don't really know if this is right or
     * not, but it doesn't seem to cause harm, and it does seem to be
     * a bad idea to have interrupts enabled here.)
     */
#if 0
    sigprocmask(SIG_SETMASK, &context->uc_sigmask, 0);
#endif
#else
    sigsetmask(context->sc_mask);
#endif

    if (CODE(code) == ILLTRAP_INST) {
	int illtrap_code;
	unsigned int inst;
	unsigned int *pc = (unsigned int *) (SC_PC(context));

	inst = *pc;

	illtrap_code = inst & 0x3fffff;

	switch (illtrap_code) {
	  case trap_PendingInterrupt:
	      arch_skip_instruction(context);
	      interrupt_handle_pending(context);
	      break;

	  case trap_Halt:
	      fake_foreign_function_call(context);
	      lose("%%primitive halt called; the party is over.\n");

	  case trap_Error:
	  case trap_Cerror:
	      interrupt_internal_error(signal, code, context,
				       illtrap_code == trap_Cerror);
	      break;

	  case trap_Breakpoint:
	      enable_some_signals();
	      handle_breakpoint(signal, CODE(code), context);
	      break;

	  case trap_FunctionEndBreakpoint:
	      enable_some_signals();
	      SC_PC(context) =
		  (long) handle_function_end_breakpoint(signal, CODE(code),
							context);
	      SC_NPC(context) = SC_PC(context) + 4;
	      break;

	  case trap_AfterBreakpoint:
	      *skipped_break_addr = trap_Breakpoint;
	      skipped_break_addr = NULL;
	      *(unsigned long *) SC_PC(context) = displaced_after_inst;
#ifdef POSIX_SIGS
	      context->uc_sigmask = orig_sigmask;
#else
	      context->sc_mask = orig_sigmask;
#endif
	      os_flush_icache((os_vm_address_t) SC_PC(context),

			      sizeof(unsigned long));
	      break;

#ifdef trap_DynamicSpaceOverflowWarning
	  case trap_DynamicSpaceOverflowWarning:
	      arch_skip_instruction(context);
	      enable_some_signals();
	      interrupt_handle_space_overflow(SymbolFunction
					      (DYNAMIC_SPACE_OVERFLOW_WARNING_HIT),
					      context);
	      break;
#endif
#ifdef trap_DynamicSpaceOverflowError
	  case trap_DynamicSpaceOverflowError:
	      arch_skip_instruction(context);
	      enable_some_signals();
	      interrupt_handle_space_overflow(SymbolFunction
					      (DYNAMIC_SPACE_OVERFLOW_ERROR_HIT),
					      context);
	      break;
#endif
	  default:
	      interrupt_handle_now(signal, code, context);
	      break;
	}
    } else if (TRAP_INST(code)) {
	if (pseudo_atomic_trap_p(context)) {
	    /* A trap instruction from a pseudo-atomic.  We just need
	       to fixup up alloc-tn to remove the interrupted flag,
	       skip over the trap instruction, and then handle the
	       pending interrupt(s). */
	    SC_REG(context, reg_ALLOC) &= ~lowtag_Mask;
	    arch_skip_instruction(context);
	    interrupt_handle_pending(context);
	}
#ifdef GENCGC
	else if (allocation_trap_p(context)) {
	    /* An allocation trap. Call the trap handler and then skip
	       this instruction */
	    handle_allocation_trap(context);
	    arch_skip_instruction(context);
	}
#endif
	else {
	    interrupt_internal_error(signal, code, context, FALSE);
	}
    } else {
	interrupt_handle_now(signal, code, context);
    }
}

void
arch_install_interrupt_handlers()
{
    interrupt_install_low_level_handler(SIGILL, sigill_handler);
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

#ifdef LINKAGE_TABLE

/* This is mostly stolen from the x86 version, with adjustments for sparc */

/*
 * Linkage entry size is 16, because we need at least 3 instruction to
 * implement a jump:
 *
 *      sethi %hi(addr), %g4
 *      jmpl  [%g4 + %lo(addr)], %g5
 *      nop
 *
 * The Sparc V9 ABI seems to use 8 words for its jump tables.  Maybe
 * we should do the same?
 */

/*
 * This had better match lisp::target-foreign-linkage-entry-size in
 * sparc/parms.lisp!  Each entry is 4 instructions long, so 16 bytes.
 */
#ifndef LinkageEntrySize
#define LinkageEntrySize (4*4)
#endif


/*
 * Define the registers to use in the linkage jump table.  Can be the
 * same.  This MUST be coordinated with resolve_linkage_tramp which
 * needs to know the register used for LINKAGE_ADDR_REG.
 *
 * Some care must be exercised when choosing these.  It has to be a
 * register that is not otherwise being used.  reg_L0 is a good
 * choice.  call_into_c trashes reg_L0 without preserving it, so we
 * can trash it in the linkage jump table.  For the linkage entries
 * that call resolve_linkage_tramp, we can use reg_L0 too because
 * resolve_linkage_tramp is always called from call_into_c.  (This is
 * enforced by having new-genesis create an entry for call_into_c, so
 * we never have to do a lookup for call_into_c.)
 *
 * The LINKAGE_ADDR_REG is important!  resolve_linkage_tramp needs to
 * be coordinated with this because that's how resolve_linkage_tramp
 * figures out what linkage entry it's being called from.
 */
#define LINKAGE_TEMP_REG        reg_L0
#define LINKAGE_ADDR_REG        reg_L0

/*
 * Insert the necessary jump instructions at the given address.
 * Return the address of the next word
 */
void *
arch_make_jump_entry(void *reloc_addr, void *target_addr)
{

    /*
     * Make JMP to function entry.
     *
     * The instruction sequence is:
     *
     *        sethi %hi(addr), temp_reg
     *        jmpl  %temp_reg + %lo(addr), %addr_reg
     *        nop
     *        nop
     *        
     */
    int *inst_ptr;
    unsigned long hi;		/* Top 22 bits of address */
    unsigned long lo;		/* Low 10 bits of address */
    unsigned int inst;

    inst_ptr = (int *) reloc_addr;

    /*
     * Split the target address into hi and lo parts for the sethi
     * instruction.  hi is the top 22 bits.  lo is the low 10 bits.
     */
    hi = (unsigned long) target_addr;
    lo = hi & 0x3ff;
    hi >>= 10;

    /*
     * sethi %hi(addr), temp_reg
     */

    inst = (0 << 30) | (LINKAGE_TEMP_REG << 25) | (4 << 22) | hi;
    *inst_ptr++ = inst;

    /*
     * jmpl [temp_reg + %lo(addr)], addr_reg
     */

    inst = (2U << 30) | (LINKAGE_ADDR_REG << 25) | (0x38 << 19)
	| (LINKAGE_TEMP_REG << 14) | (1 << 13) | lo;
    *inst_ptr++ = inst;

    /* nop (really sethi 0, %g0) */

    inst = (0 << 30) | (0 << 25) | (4 << 22) | 0;

    *inst_ptr++ = inst;
    *inst_ptr++ = inst;

    os_flush_icache((os_vm_address_t) reloc_addr,
		    (char *) inst_ptr - (char *) reloc_addr);
    return reloc_addr;
}

void
arch_make_linkage_entry(long linkage_entry, void *target_addr, long type)
{
    int *reloc_addr = (int *) (FOREIGN_LINKAGE_SPACE_START

			       + linkage_entry * LinkageEntrySize);

    if (type == 1) {		/* code reference */
	arch_make_jump_entry(reloc_addr, target_addr);
    } else if (type == 2) {
	*(unsigned long *) reloc_addr = (unsigned long) target_addr;
    }
}

/* Make a the entry a jump to resolve_linkage_tramp. */

extern void resolve_linkage_tramp(void);

void
arch_make_lazy_linkage(long linkage_entry)
{
    arch_make_linkage_entry(linkage_entry, (void *) resolve_linkage_tramp, 1);
}

/* Get linkage entry.  We're given the return address which should be
   the address of the jmpl instruction (2nd word) of the linkage
   entry.  Figure out which entry this address belong to. */

long
arch_linkage_entry(unsigned long retaddr)
{
    return (retaddr - (FOREIGN_LINKAGE_SPACE_START))
	/ LinkageEntrySize;
}
#endif /* LINKAGE_TABLE */
