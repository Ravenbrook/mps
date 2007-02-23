/*

 $Header: /project/cmucl/cvsroot/src/lisp/ppc-arch.c,v 1.9 2006/02/19 22:56:35 rtoy Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>

#include "arch.h"
#include "lisp.h"
#include "internals.h"
#include "globals.h"
#include "validate.h"
#include "os.h"
#include "lispregs.h"
#include "signal.h"
#include "interrupt.h"
#include "interr.h"

  /* The header files may not define PT_DAR/PT_DSISR.  This definition
     is correct for all versions of ppc linux >= 2.0.30

     As of DR2.1u4, MkLinux doesn't pass these registers to signal
     handlers correctly; a patch is necessary in order to (partially)
     correct this.

     Even with the patch, the DSISR may not have its 'write' bit set
     correctly (it tends not to be set if the fault was caused by
     something other than a protection violation.)

     Caveat callers.  */

#ifndef PT_DAR
#define PT_DAR		41
#endif

#ifndef PT_DSISR
#define PT_DSISR	42
#endif

/* 
 * A macro to generate the instruction
 *
 * twllei r0, code
 *
 * This is what the ppc port uses to signal various traps like
 * breakpoints and stuff.
 */
#define TWLLEI_R0(code) ((3<<26) | (6 << 21) | code)

char *
arch_init(void)
{
    return "lisp.core";
}

os_vm_address_t arch_get_bad_addr(HANDLER_ARGS)
{
    os_vm_address_t addr;

    addr = (os_vm_address_t) SC_REG(context, PT_DAR);
    return addr;
}


void
arch_skip_instruction(os_context_t * context)
{
    /* Skip the offending instruction */
    SC_PC(context) += 4;
}

unsigned char *
arch_internal_error_arguments(os_context_t * scp)
{
    return (unsigned char *) (SC_PC(scp) + 4);
}

boolean arch_pseudo_atomic_atomic(os_context_t * scp)
{
    return (SC_REG(scp, reg_ALLOC) & 4);
}

#define PSEUDO_ATOMIC_INTERRUPTED_BIAS 0x7f000000

void
arch_set_pseudo_atomic_interrupted(os_context_t * scp)
{
#if 0
    SC_REG(scp, reg_NL3) += PSEUDO_ATOMIC_INTERRUPTED_BIAS;
#else
    SC_REG(scp, reg_ALLOC) |= 1;
#endif
}

unsigned long
arch_install_breakpoint(void *pc)
{
    unsigned long *ptr = (unsigned long *) pc;
    unsigned long result = *ptr;

    /* 
     * Insert a twllei r0, trap_Breakpoint instruction.
     */
    *ptr = TWLLEI_R0(trap_Breakpoint);
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned long));

    return result;
}

void
arch_remove_breakpoint(void *pc, unsigned long orig_inst)
{
    *(unsigned long *) pc = orig_inst;
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned long));
}

static unsigned long *skipped_break_addr, displaced_after_inst;
static sigset_t orig_sigmask;

void
arch_do_displaced_inst(os_context_t * scp, unsigned long orig_inst)
{
    unsigned int *pc = (unsigned long *) SC_PC(scp);

    orig_sigmask = scp->uc_sigmask;
    sigemptyset(&scp->uc_sigmask);
    FILLBLOCKSET(&scp->uc_sigmask);

    /* Put the original instruction back */
    *pc = orig_inst;
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned int));

    skipped_break_addr = pc;

    /*
     * Replace the next instruction with a 
     * twllei r0, trap_AfterBreakpoint 
     */
    displaced_after_inst = *++pc;
    *pc = TWLLEI_R0(trap_AfterBreakpoint);
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned int));

    sigreturn(scp);
}

#ifdef GENCGC
/*
 * Return non-zero if the current instruction is an allocation trap
 */
static int
allocation_trap_p(os_context_t * context)
{
    int result;
    unsigned int *pc;
    unsigned inst;
    unsigned opcode;
    unsigned src;
    unsigned dst;

    result = 0;

    /*
     * First, the instruction has to be a TWLGE temp, NL3, which as the
     * format.
     * | 6| 5| 5 | 5 | 10|1|  width
     * |31|5 |dst|src|  4|0|  field
     */
    pc = (unsigned int *) SC_PC(context);
    inst = *pc;

#if 0
    fprintf(stderr, "allocation_trap_p at %p:  inst = 0x%08x\n", pc, inst);
#endif

    opcode = inst >> 26;
    src = (inst >> 11) & 0x1f;
    dst = (inst >> 16) & 0x1f;
    if ((opcode == 31) && (src == reg_NL3) && (5 == ((inst >> 21) & 0x1f))
	&& (4 == ((inst >> 1) & 0x3ff))) {
	/*
	 * We got the instruction.  Now, look back to make sure it was
	 * proceeded by what we expected.  2 instructions back should be
	 * an ADD or ADDI instruction.
	 */
	unsigned int add_inst;

	add_inst = pc[-2];
#if 0
	fprintf(stderr, "   add inst at %p:  inst = 0x%08x\n",
		pc - 2, add_inst);
#endif
	opcode = add_inst >> 26;
	if ((opcode == 31) && (266 == ((add_inst >> 1) & 0x1ff))) {
	    return 1;
	} else if ((opcode == 14)) {
	    return 1;
	} else {
	    fprintf(stderr,
		    "Whoa! Got allocation trap not preceeded by an ADD or ADDI instruction: 0x%08x\n",
		    add_inst);
	}
    }
    return 0;
}

/*
 * Use this function to enable the minimum number of signals we need
 * when our trap handler needs to call Lisp code that might cons.  For
 * consing to work with gencgc, we need to be able to trap the SIGILL
 * signal to perform allocation.
 */
void
enable_some_signals()
{
    sigset_t sigs;

#if 0
    fprintf(stderr, "Enabling some signals\n");
#endif
#if 0
    sigprocmask(SIG_SETMASK, &context->uc_sigmask, 0);
#else
    sigemptyset(&sigs);
    sigaddset(&sigs, SIGILL);
    sigaddset(&sigs, SIGBUS);
    sigprocmask(SIG_UNBLOCK, &sigs, NULL);
#endif
#if 0
    fprintf(stderr, "Some signals enabled\n");
#endif
}

void
handle_allocation_trap(os_context_t * context)
{
    unsigned int *pc;
    unsigned int inst;
    unsigned int or_inst;
    unsigned int target;
    unsigned int opcode;
    int size;
    int immed;
    boolean were_in_lisp;
    char *memory;
    sigset_t block;

    target = 0;
    size = 0;

#if 0
    fprintf(stderr, "In handle_allocation_trap\n");
#endif
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
     * Look at current instruction: TWNE temp, NL3. We're here because
     * temp > NL3 and temp is the end of the allocation, and NL3 is
     * current-region-end-addr.
     *
     * We need to adjust temp and alloc-tn.
     */

    pc = (unsigned int *) SC_PC(context);
    inst = pc[0];
    target = (inst >> 16) & 0x1f;

#if 0
    fprintf(stderr, "handle_allocation_trap at %p:\n", pc);
    fprintf(stderr, "  trap inst = 0x%08x\n", inst);
    fprintf(stderr, "  target reg = %s\n", lisp_register_names[target]);
#endif
    /*
     * Go back and look at the add/addi instruction.  The second src arg
     * is the size of the allocation.  Get it and call alloc to allocate
     * new space.
     */
    inst = pc[-2];
    opcode = inst >> 26;
#if 0
    fprintf(stderr, "  add inst  = 0x%08x, opcode = %d\n", inst, opcode);
#endif
    if (opcode == 14) {
	/*
	 * ADDI temp-tn, alloc-tn, size 
	 *
	 * Extract the size
	 */
	size = (inst & 0xffff);
    } else if (opcode == 31) {
	/*
	 * ADD temp-tn, alloc-tn, size-tn
	 *
	 * Extract the size
	 */
	int reg;

	reg = (inst >> 11) & 0x1f;
#if 0
	fprintf(stderr, "  add, reg = %s\n", lisp_register_names[reg]);
#endif
	size = SC_REG(context, reg);
    }
#if 0
    fprintf(stderr, "Alloc %d to %s\n", size, lisp_register_names[target]);
#endif

    /*
     * Well, maybe not.  sigill_handler probably shouldn't be unblocking
     * all signals.  So, let's enable just the signals we need.  Since
     * alloc might call GC, we need to have SIGILL enabled so we can do
     * allocation.  Do we need more?
     */
    enable_some_signals();

#if 0
    fprintf(stderr, "Ready to alloc\n");
    fprintf(stderr, "free_pointer = 0x%08x\n",
	    current_dynamic_space_free_pointer);
#endif
    /*
     * alloc-tn was incremented by size.  Need to decrement it by size to restore it's original value.
     */
    current_dynamic_space_free_pointer =
	(lispobj *) ((long) current_dynamic_space_free_pointer - size);
#if 0
    fprintf(stderr, "free_pointer = 0x%08x new\n",
	    current_dynamic_space_free_pointer);
#endif

    memory = (char *) alloc(size);

#if 0
    fprintf(stderr, "alloc returned %p\n", memory);
    fprintf(stderr, "free_pointer = 0x%08x\n",
	    current_dynamic_space_free_pointer);
#endif

    /* 
     * The allocation macro wants the result to point to the end of the
     * object!
     */
    memory += size;
#if 0
    fprintf(stderr, "object end at %p\n", memory);
#endif
    SC_REG(context, target) = (unsigned long) memory;
    SC_REG(context, reg_ALLOC) =
	(unsigned long) current_dynamic_space_free_pointer;

    if (were_in_lisp) {
	undo_fake_foreign_function_call(context);
    }


}
#endif

static void
sigill_handler(HANDLER_ARGS)
{
    int badinst;
    int opcode;

    HANDLER_GET_CONTEXT SAVE_CONTEXT();

    sigprocmask(SIG_SETMASK, &context->uc_sigmask, 0);
    opcode = *((int *) SC_PC(context));

#if 0
    printf("SIGILL entry:  opcode = 0x%08x\n", opcode);
    fflush(stdout);
#endif

    if (opcode == ((3 << 26) | (0x18 << 21) | (reg_NL3 << 16))) {
	/* Got a twnei reg_NL3,0 - check for deferred interrupt */
#if 1
	/* Clear the pseudo-atomic-interrupted bit */
	SC_REG(context, reg_ALLOC) &= ~1;
#else
	(SC_REG(context, reg_ALLOC) -= PSEUDO_ATOMIC_INTERRUPTED_BIAS);
#endif
	arch_skip_instruction(context);
	interrupt_handle_pending(context);
#ifdef DARWIN
	/* Work around G5 bug; fix courtesy gbyers via chandler */
	sigreturn(context);
#endif
	return;
    }

    /* Is this an allocation trap? */
#ifdef GENCGC
    if (allocation_trap_p(context)) {
	handle_allocation_trap(context);
	arch_skip_instruction(context);
#ifdef DARWIN
	sigreturn(context);
#endif
	return;
    }
#endif

    if ((opcode >> 16) == ((3 << 10) | (6 << 5))) {
	/* twllei reg_ZERO,N will always trap if reg_ZERO = 0 */
	int trap = opcode & 0x1f, extra = (opcode >> 5) & 0x1f;

#if 0
	printf("SIGILL:  TWLLEI, code = %d\n", trap);
	fflush(stdout);
#endif

	switch (trap) {
	  case trap_Halt:
	      fake_foreign_function_call(context);
	      lose("%%primitive halt called; the party is over.\n");

	  case trap_Error:
	  case trap_Cerror:
	      interrupt_internal_error(signal, code, context,
				       trap == trap_Cerror);
	      break;

	  case trap_PendingInterrupt:
	      arch_skip_instruction(context);
	      interrupt_handle_pending(context);
	      break;

	  case trap_Breakpoint:
#if 0
	      printf("trap_Breakpoint\n");
	      fflush(stdout);
#endif
	      handle_breakpoint(signal, code, context);
	      break;

	  case trap_FunctionEndBreakpoint:
#if 0
	      printf("trap_FunctionEndBreakpoint\n");
	      fflush(stdout);
#endif
	      SC_PC(context) =
		  (int) handle_function_end_breakpoint(signal, code, context);
	      break;

	  case trap_AfterBreakpoint:
#if 0
	      fprintf(stderr, "trap_AfterBreakpoint: break_addr = %p\n",
		      skipped_break_addr);
	      fprintf(stderr, " CSP  = %p\n",
		      (void *) SC_REG(context, reg_CSP));
	      fprintf(stderr, " CFP  = %p\n",
		      (void *) SC_REG(context, reg_CFP));
	      fprintf(stderr, " OCFP = %p\n",
		      (void *) SC_REG(context, reg_OCFP));
#endif
	      /* Put our breakpoint instruction back in */
	      *skipped_break_addr = TWLLEI_R0(trap_Breakpoint);
	      skipped_break_addr = NULL;
	      *(unsigned long *) SC_PC(context) = displaced_after_inst;
	      context->uc_sigmask = orig_sigmask;

	      os_flush_icache((os_vm_address_t) SC_PC(context),
			      sizeof(unsigned long));
	      break;

	  default:
	      interrupt_handle_now(signal, code, context);
	      break;
	}
#ifdef DARWIN
	/* Work around G5 bug; fix courtesy gbyers via chandler */
	sigreturn(context);
#endif
	return;
    }
    if (((opcode >> 26) == 3) && (((opcode >> 21) & 31) == 24)) {
	interrupt_internal_error(signal, code, context, 0);
#ifdef DARWIN
	/* Work around G5 bug; fix courtesy gbyers via chandler */
	sigreturn(context);
#endif
	return;
    }

    interrupt_handle_now(signal, code, context);
#ifdef DARWIN
    /* Work around G5 bug; fix courtesy gbyers via chandler */
    sigreturn(context);
#endif
}


void
arch_install_interrupt_handlers()
{
    interrupt_install_low_level_handler(SIGILL, sigill_handler);
    interrupt_install_low_level_handler(SIGTRAP, sigill_handler);
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

void
ppc_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    os_vm_address_t end =
	(os_vm_address_t) ((int) (address + length + (32 - 1)) & ~(32 - 1));
    extern void ppc_flush_cache_line(os_vm_address_t);

    while (address < end) {
	ppc_flush_cache_line(address);
	address += 32;
    }
}

#ifdef LINKAGE_TABLE
/* Linkage tables for PowerPC
 *
 * Linkage entry size is 16, because we need at least 4 instructions to
 * implement a jump.
 */

/*
 * This had better match lisp::target-foreign-linkage-entry-size in
 * ppco/parms.lisp!  Each entry is 6 instructions long, so at least
 * 24 bytes.
 */
#ifndef LinkageEntrySize
#define LinkageEntrySize (8*4)
#endif

/*
 * Define the registers to use in the linkage jump table. Can be the
 * same. Some care must be exercised when choosing these. It has to be
 * a register that is not otherwise being used. reg_NFP is a good
 * choice. call_into_c trashes reg_NFP without preserving it, so we can
 * trash it in the linkage jump table.
 */
#define LINKAGE_TEMP_REG        reg_NFP
#define LINKAGE_ADDR_REG        reg_A0

/*
 * Insert the necessary jump instructions at the given address.
 */
void
arch_make_jump_entry(void *reloc_addr, void *target_addr)
{
    /*
     * Make JMP to function entry.
     *
     * The instruction sequence is:
     *
     *        addis temp, 0, (hi part of reloc)
     *        ori   temp, temp, (lo part of reloc)
     *        addis addr, 0, (hi part of addr)
     *        ori   addr, addr, (low part of addr)
     *        mtctr addr
     *        bctr
     *        
     */
    int *inst_ptr;
    unsigned long hi;		/* Top 16 bits of address */
    unsigned long lo;		/* Low 16 bits of address */
    unsigned int inst;

    inst_ptr = (int *) reloc_addr;

    /*
     * Split the target address into hi and lo parts for the addis/ori
     * instructions.
     */
    hi = (unsigned long) reloc_addr;
    lo = hi & 0xffff;
    hi >>= 16;

    /*
     * addis 3, 0, (hi part)
     */
    inst = (15 << 26) | (LINKAGE_ADDR_REG << 21) | (0 << 16) | hi;
    *inst_ptr++ = inst;

    /*
     * ori 3, 3, (lo part)
     */

    inst =
	(24 << 26) | (LINKAGE_ADDR_REG << 21) | (LINKAGE_ADDR_REG << 16) | lo;
    *inst_ptr++ = inst;

    /*
     * Split the target address into hi and lo parts for the addis/ori
     * instructions.
     */

    hi = (unsigned long) target_addr;
    lo = hi & 0xffff;
    hi >>= 16;

    /*
     * addis 13, 0, (hi part)
     */

    inst = (15 << 26) | (LINKAGE_TEMP_REG << 21) | (0 << 16) | hi;
    *inst_ptr++ = inst;

    /*
     * ori 13, 13, (lo part)
     */

    inst =
	(24 << 26) | (LINKAGE_TEMP_REG << 21) | (LINKAGE_TEMP_REG << 16) | lo;
    *inst_ptr++ = inst;

    /*
     * mtctr 13
     */

    inst = (31 << 26) | (LINKAGE_TEMP_REG << 21) | (9 << 16) | (467 << 1);
    *inst_ptr++ = inst;

    /*
     * bctr
     */

    inst = (19 << 26) | (20 << 21) | (528 << 1);
    *inst_ptr++ = inst;


    *inst_ptr++ = inst;

    os_flush_icache((os_vm_address_t) reloc_addr,
		    (char *) inst_ptr - (char *) reloc_addr);
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
#endif
