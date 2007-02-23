/* $Header: /project/cmucl/cvsroot/src/lisp/mips-assem.S,v 1.9 2002/08/23 17:01:02 pmai Exp $ */
#ifdef mach
#include <machine/regdef.h>
#else
#ifdef irix
#include <sys/regdef.h>
#endif
#endif

#include "internals.h"
#include "lispregs.h"
#include "globals.h"

#if !defined(s8)
#define s8 $30
#endif

/*
 * Function to transfer control into lisp.
 */
	.text
	.globl	call_into_lisp
	.ent	call_into_lisp
call_into_lisp:
#define framesize 12*4
	subu	sp, framesize
	.frame	sp, framesize, ra
	/* Save all the C regs. */
	.mask	0xc0ff0000, 0
	sw	ra, framesize(sp)
	sw	s8, framesize-4(sp)
	sw	s7, framesize-12(sp)
	sw	s6, framesize-16(sp)
	sw	s5, framesize-20(sp)
	sw	s4, framesize-24(sp)
	sw	s3, framesize-28(sp)
	sw	s2, framesize-32(sp)
	sw	s1, framesize-36(sp)
	sw	s0, framesize-40(sp)

	/* Clear descriptor regs */
	move	t0, zero
	move	t1, zero
	move	t2, zero
	move	t3, zero
	move	t4, zero
	move	t5, zero
	move	t6, zero
	move	t7, zero
	move	t8, zero
	move	s0, zero
	move	s1, zero
	move	s2, zero
	move	s3, zero
	move	ra, zero

	li	reg_NIL, NIL

	/* Start pseudo-atomic. */
	.set	noreorder
	li	reg_NL4, -1
	li	reg_ALLOC, 1
        .set    reorder

	/* No longer in foreign call. */
	sw	zero, foreign_function_call_active

	/* Load the allocation pointer, preserving the low-bit of alloc */
	lw	v0, current_dynamic_space_free_pointer
	add	reg_ALLOC, v0

	/* Load the rest of the LISP state. */
	lw	reg_BSP, current_binding_stack_pointer
	lw	reg_CSP, current_control_stack_pointer
	lw	reg_OCFP, current_control_frame_pointer

	/* Check for interrupt */
        .set    noreorder
	add	reg_ALLOC, reg_NL4
	.set	reorder

	/* Pass in args */
	move	reg_LEXENV, $4
	move	reg_CFP, $5
	sll	reg_NARGS, $6, 2
	lw	reg_A0, 0(reg_CFP)
	lw	reg_A1, 4(reg_CFP)
	lw	reg_A2, 8(reg_CFP)
	lw	reg_A3, 12(reg_CFP)
	lw	reg_A4, 16(reg_CFP)
	lw	reg_A5, 20(reg_CFP)

	/* Calculate LRA */
	la	reg_LRA, lra + type_OtherPointer

	/* Indirect closure */
	lw	reg_CODE, 4-1(reg_LEXENV)

	/* Jump into lisp land. */
	addu	reg_LIP, reg_CODE, 6*4 - type_FunctionPointer
	j	reg_LIP

	.set	noreorder
	.align	3
#ifdef irix
.globl  mipsmungelra /* for our munging afterwards in irix-asm-munge */
mipsmungelra:
#endif
lra:
#ifndef irix
	.word	type_ReturnPcHeader
#else
        break 11
#endif

	/* Multiple value return spot, clear stack */
	move	reg_CSP, reg_OCFP
	nop

	/* Set pseudo-atomic flag. */
	li	reg_NL4, -1
	addu	reg_ALLOC, 1
	.set	reorder

	/* Pass one return value back to C land. */
	move	v0, reg_A0

	/* Save LISP registers. */
	subu	reg_NL0, reg_ALLOC, 1
	sw	reg_NL0, current_dynamic_space_free_pointer
	sw	reg_BSP, current_binding_stack_pointer
	sw	reg_CSP, current_control_stack_pointer
	sw	reg_CFP, current_control_frame_pointer

	/* Back in foreign function call */
	sw	reg_CFP, foreign_function_call_active

	/* Check for interrupt */
	.set	noreorder
	add	reg_ALLOC, reg_NL4
	.set	reorder

	/* Restore C regs */
	lw	ra, framesize(sp)
	lw	s8, framesize-4(sp)
	lw	s7, framesize-12(sp)
	lw	s6, framesize-16(sp)
	lw	s5, framesize-20(sp)
	lw	s4, framesize-24(sp)
	lw	s3, framesize-28(sp)
	lw	s2, framesize-32(sp)
	lw	s1, framesize-36(sp)
	lw	s0, framesize-40(sp)

	/* Restore C stack. */
	addu	sp, framesize

	/* Back we go. */
	j	ra

	.end	call_into_lisp

/*
 * Transfering control from Lisp into C
 */
	.text
	.globl	call_into_c
	.ent	call_into_c
call_into_c:
	/* Set up a stack frame. */
	move	reg_OCFP, reg_CFP
	move	reg_CFP, reg_CSP
	addu	reg_CSP, reg_CFP, 32
	sw	reg_OCFP, 0(reg_CFP)
	subu	reg_NL4, reg_LIP, reg_CODE
	addu	reg_NL4, type_OtherPointer
	sw	reg_NL4, 4(reg_CFP)
	sw	reg_CODE, 8(reg_CFP)
#ifdef irix
        sw      gp, 12(reg_CFP)
#endif

	/* Note: the C stack is already set up. */

	/* Set the pseudo-atomic flag. */
	.set	noreorder
	li	reg_NL4, -1
	addu	reg_ALLOC, 1
	.set	reorder

	/* Save lisp state. */
	subu	t0, reg_ALLOC, 1
	sw	t0, current_dynamic_space_free_pointer
	sw	reg_BSP, current_binding_stack_pointer
	sw	reg_CSP, current_control_stack_pointer
	sw	reg_CFP, current_control_frame_pointer

	/* Mark us as in C land. */
	sw	reg_CSP, foreign_function_call_active

	/* Were we interrupted? */
	.set	noreorder
	add	reg_ALLOC, reg_NL4
	.set	reorder

	/* Into C land we go. */
#ifdef irix
	move	t9, reg_CFUNC
#endif
	jal	reg_CFUNC
#ifdef irix
        /* we rely on assembler to insert a nop between the jal and this */
        lw      gp, 12(reg_CFP)
#endif

	/* Clear unsaved descriptor regs */
	move	t0, zero
	move	t1, zero
	move	t2, zero
	move	t3, zero
	move	t4, zero
	move	t5, zero
	move	t6, zero
	move	t7, zero
	move	t8, zero
	move	s0, zero
	move	s2, zero
	move	s3, zero
	move	ra, zero

	/* Turn on pseudo-atomic. */
	.set	noreorder
	li	reg_NL4, -1
	li	reg_ALLOC, 1
	.set	reorder

	/* Mark us at in Lisp land. */
	sw	zero, foreign_function_call_active

	/* Restore ALLOC, preserving pseudo-atomic-atomic */
	lw	a0, current_dynamic_space_free_pointer
	addu	reg_ALLOC, a0

	/* Check for interrupt */
	.set	noreorder
	add	reg_ALLOC, reg_NL4
	.set	reorder

	/* Restore LRA & CODE (they may have been GC'ed) */
	lw	reg_CODE, 8(reg_CFP)
	lw	a0, 4(reg_CFP)
	subu	a0, type_OtherPointer
	addu	reg_LIP, reg_CODE, a0

	/* Reset the lisp stack. */
	/* Note: OCFP and CFP are in saved regs. */
	move	reg_CSP, reg_CFP
	move	reg_CFP, reg_OCFP

	/* Return to LISP. */
	j	reg_LIP

	.end	call_into_c

	.text
	.globl	start_of_tramps
start_of_tramps:

/*
 * The undefined-function trampoline.
 */
        .text
        .globl  undefined_tramp
        .ent    undefined_tramp
undefined_tramp:
        break   trap_Error
#ifdef irix
        break 11
        break 11
#else
        /* Number of argument bytes */
        .byte   4
        .byte   UNDEFINED_SYMBOL_ERROR
        /* Escape to create 16bit BE number from following two values */
        .byte   254
        /* SC_OFFSET(sc_DescriptorReg,reg_FDEFN) */
        .byte   (0xc0 + sc_DescriptorReg)
        .byte   1
        .align 2
#endif
        .end    undefined_tramp

/*
 * The closure trampoline.
 */
        .text
        .globl  closure_tramp
        .ent    closure_tramp
closure_tramp:
        lw      reg_LEXENV, FDEFN_FUNCTION_OFFSET(reg_FDEFN)
        lw      reg_L0, CLOSURE_FUNCTION_OFFSET(reg_LEXENV)
        addu    reg_LIP, reg_L0, FUNCTION_CODE_OFFSET
        j       reg_LIP
        .end    closure_tramp

	.text
	.globl	end_of_tramps
end_of_tramps:


/*
 * Function-end breakpoint magic.
 */

	.text
	.align	2
	.set	noreorder
	.globl	function_end_breakpoint_guts
function_end_breakpoint_guts:
#ifndef irix
	.word	type_ReturnPcHeader
#else
        break 11
#endif
	beq	zero, zero, 1f
	nop
	move	reg_OCFP, reg_CSP
	addu	reg_CSP, 4
	li	reg_NARGS, 4
	move	reg_A1, reg_NIL
	move	reg_A2, reg_NIL
	move	reg_A3, reg_NIL
	move	reg_A4, reg_NIL
	move	reg_A5, reg_NIL
1:

	.globl	function_end_breakpoint_trap
function_end_breakpoint_trap:
	break	trap_FunctionEndBreakpoint
	beq	zero, zero, 1b
	nop

	.globl	function_end_breakpoint_end
function_end_breakpoint_end:
	.set	reorder


	.text
	.align	2
	.globl	call_on_stack
	.ent	call_on_stack
call_on_stack:
	subu	sp, a1, 16
	jal	a0
	break	0
	.end	call_on_stack

	.globl	save_state
	.ent	save_state
save_state:
	subu	sp, 40
	.frame	sp, 40, ra
	/* Save all the C regs. */
	.mask	0xc0ff0000, 0
	sw	ra, 40(sp)
	sw	s8, 40-4(sp)
	sw	s7, 40-8(sp)
	sw	s6, 40-12(sp)
	sw	s5, 40-16(sp)
	sw	s4, 40-20(sp)
	sw	s3, 40-24(sp)
	sw	s2, 40-28(sp)
	sw	s1, 40-32(sp)
	sw	s0, 40-36(sp)

	/* Should also save the floating point state. */

	move	t0, a0
	move	a0, sp

	jal	t0

_restore_state:

	lw	ra, 40(sp)
	lw	s8, 40-4(sp)
	lw	s7, 40-8(sp)
	lw	s6, 40-12(sp)
	lw	s5, 40-16(sp)
	lw	s4, 40-20(sp)
	lw	s3, 40-24(sp)
	lw	s2, 40-28(sp)
	lw	s1, 40-32(sp)
	lw	s0, 40-36(sp)

	addu	sp, 40
	j	ra

	.globl	restore_state
restore_state:
	move	sp, a0
	move	v0, a1
	j	_restore_state
	.end	save_state
