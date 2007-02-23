/*

 $Header: /project/cmucl/cvsroot/src/lisp/breakpoint.c,v 1.18 2005/09/15 18:26:51 rtoy Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>
#include <signal.h>

#include "lisp.h"
#include "os.h"
#include "internals.h"
#include "interrupt.h"
#include "arch.h"
#include "lispregs.h"
#include "globals.h"
#include "alloc.h"
#include "breakpoint.h"
#if defined GENCGC
#include "gencgc.h"
#endif

/*
 * See MAKE-BOGUS-LRA in code/debug-int.lisp for these values.  (We
 * really should generate these from the Lisp code.)
 */
#define REAL_LRA_SLOT 0
#ifndef i386
#define KNOWN_RETURN_P_SLOT 1
#define BOGUS_LRA_CONSTANTS 2
#else
#define KNOWN_RETURN_P_SLOT 2
#define BOGUS_LRA_CONSTANTS 3
#endif

static void *
compute_pc(lispobj code_obj, int pc_offset)
{
    struct code *code;

    code = (struct code *) PTR(code_obj);
    return (void *) ((char *) code + HeaderValue(code->header) * sizeof(lispobj)
		     + pc_offset);
}

unsigned long
breakpoint_install(lispobj code_obj, int pc_offset)
{
    return arch_install_breakpoint(compute_pc(code_obj, pc_offset));
}

void
breakpoint_remove(lispobj code_obj, int pc_offset, unsigned long orig_inst)
{
    arch_remove_breakpoint(compute_pc(code_obj, pc_offset), orig_inst);
}

void
breakpoint_do_displaced_inst(os_context_t * scp, unsigned long orig_inst)
{
#if !defined(hpux) && !defined(irix) && !defined(i386)
    undo_fake_foreign_function_call(scp);
#endif
    arch_do_displaced_inst(scp, orig_inst);
}

#if !defined(i386)
static lispobj
find_code(os_context_t * scp)
{
#ifdef reg_CODE
    lispobj code = SC_REG(scp, reg_CODE), header;

    if (LowtagOf(code) != type_OtherPointer)
	return NIL;

    header = *(lispobj *) (code - type_OtherPointer);

    if (TypeOf(header) == type_CodeHeader)
	return code;
    else
	return code - HeaderValue(header) * sizeof(lispobj);
#else
    return NIL;
#endif
}
#endif

#if defined(i386)
static lispobj
find_code(os_context_t * scp)
{
    lispobj *codeptr = component_ptr_from_pc(SC_PC(scp));

    if (codeptr == NULL)
	return NIL;
    else
	return (lispobj) codeptr | type_OtherPointer;
}
#endif

static int
compute_offset(os_context_t * scp, lispobj code, boolean function_end)
{
    if (code == NIL)
	return 0;
    else {
	unsigned long code_start;
	struct code *codeptr = (struct code *) PTR(code);

#ifdef parisc
	unsigned long pc = SC_PC(scp) & ~3;
#else
	unsigned long pc = SC_PC(scp);
#endif

	code_start = (unsigned long) codeptr
	    + HeaderValue(codeptr->header) * sizeof(lispobj);
	if (pc < code_start)
	    return 0;
	else {
	    int offset = pc - code_start;

	    if (offset >= codeptr->code_size) {
		if (function_end) {
#if defined(sparc) || defined(DARWIN)
		    /*
		     * We're in a function end breakpoint.  Compute the
		     * offset from the (known) breakpoint location and the
		     * beginning of the breakpoint guts.  (See *-assem.S.)
		     *
		     * Then make the offset negative so the caller knows
		     * that the offset is not from the code object.
		     */
		    extern char function_end_breakpoint_trap;
		    extern char function_end_breakpoint_guts;

		    offset =
			&function_end_breakpoint_trap -
			&function_end_breakpoint_guts;
		    return make_fixnum(-offset);
#else
		    return 0;
#endif
		} else {
		    return 0;
		}
	    } else {
		return make_fixnum(offset);
	    }
	}
    }
}

#ifndef i386
void
handle_breakpoint(int signal, int subcode, os_context_t * scp)
{
    lispobj code;

    fake_foreign_function_call(scp);

    code = find_code(scp);

    funcall3(SymbolFunction(HANDLE_BREAKPOINT),
	     compute_offset(scp, code, 0), code, alloc_sap(scp));

    undo_fake_foreign_function_call(scp);
}
#else
void
handle_breakpoint(int signal, int subcode, os_context_t * scp)
{
    lispobj code, scp_sap = alloc_sap(scp);

    fake_foreign_function_call(scp);

    code = find_code(scp);

    /*
     * Don't disallow recursive breakpoint traps.  Otherwise, we can't
     * use debugger breakpoints anywhere in here.
     */

#if defined POSIX_SIGS
    sigprocmask(SIG_SETMASK, &scp->uc_sigmask, NULL);
#else
    sigsetmask(scp->sc_mask);
#endif
    funcall3(SymbolFunction(HANDLE_BREAKPOINT),
	     compute_offset(scp, code, 0), code, scp_sap);

    undo_fake_foreign_function_call(scp);
}
#endif

#ifndef i386
void *
handle_function_end_breakpoint(int signal, int subcode, os_context_t * scp)
{
    lispobj code, lra;
    struct code *codeptr;
    int offset;

    fake_foreign_function_call(scp);

    code = find_code(scp);
    codeptr = (struct code *) PTR(code);
    offset = compute_offset(scp, code, 1);
#if 0
    printf("handle_function_end:\n");
    printf(" code    = 0x%08x\n", code);
    printf(" codeptr = %p\n", codeptr);
    printf(" offset  = %d\n", fixnum_value(offset));
    fflush(stdout);
#endif

    if (offset < 0) {
	/*
	 * We were in the function end breakpoint.  Which means we are
	 * in a bogus LRA, so compute where the code-component of this
	 * bogus lra object starts.  Adjust code, and codeptr
	 * appropriately so the breakpoint handler can do the right
	 * thing.
	 */
	unsigned int pc;

	pc = SC_PC(scp);

	offset = -offset;
	/*
	 * Some magic here.  pc points to the trap instruction.  The
	 * offset gives us where the function_end_breakpoint_guts
	 * begins.  But we need to back up some more to get to the
	 * code-component object.  The magic 2 below is 
	 */
	code = pc - fixnum_value(offset);
	code -= sizeof(struct code) + BOGUS_LRA_CONSTANTS * sizeof(lispobj);

	code += type_OtherPointer;
	codeptr = (struct code *) PTR(code);
#if 0
	printf("  pc   = 0x%08x\n", pc);
	printf("  code    = 0x%08x\n", code);
	printf("  codeptr = %p\n", codeptr);
	fflush(stdout);
#endif
    }

    funcall3(SymbolFunction(HANDLE_BREAKPOINT), offset, code, alloc_sap(scp));

    /*
     * Breakpoint handling done, so get the real LRA where we're
     * supposed to return to so we can return there.
     */
    lra = codeptr->constants[REAL_LRA_SLOT];
#ifdef reg_CODE
    /*
     * With the known-return convention, we definitely do NOT want to
     * mangle the CODE register because it isn't pointing to the bogus
     * LRA but to the actual routine.
     */
    if (codeptr->constants[KNOWN_RETURN_P_SLOT] == NIL)
	SC_REG(scp, reg_CODE) = lra;
#endif
    undo_fake_foreign_function_call(scp);
    return (void *) (lra - type_OtherPointer + sizeof(lispobj));
}
#else
void *
handle_function_end_breakpoint(int signal, int subcode, os_context_t * scp)
{
    lispobj code, scp_sap = alloc_sap(scp);
    struct code *codeptr;

    fake_foreign_function_call(scp);

    code = find_code(scp);
    codeptr = (struct code *) PTR(code);

    /*
     * Don't disallow recursive breakpoint traps.  Otherwise, we can't
     * use debugger breakpoints anywhere in here.
     */

#if defined POSIX_SIGS
    sigprocmask(SIG_SETMASK, &scp->uc_sigmask, NULL);
#else
    sigsetmask(scp->sc_mask);
#endif
    funcall3(SymbolFunction(HANDLE_BREAKPOINT),
	     compute_offset(scp, code, 1), code, scp_sap);

    undo_fake_foreign_function_call(scp);

    return compute_pc(codeptr->constants[REAL_LRA_SLOT],
		      fixnum_value(codeptr->constants[REAL_LRA_SLOT + 1]));
}
#endif
