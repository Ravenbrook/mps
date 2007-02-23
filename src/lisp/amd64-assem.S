### amd64-assem.S -*- Mode: Asm; -*-
/**
 * $Header: /project/cmucl/cvsroot/src/lisp/amd64-assem.S,v 1.10 2004/07/27 22:03:53 cwang Exp $
 *
 * Authors:	Paul F. Werkowski <pw@snoopy.mv.com>
 *		Douglas T. Crosher
 *		Cheuksan Edward Wang
 *
 * This code was written to support the port of CMU Common Lisp
 * to the AMD64 ISA and the Linux operating system. The
 * author has placed this code in the public domain September 1996.
 *
 */


#include "x86-validate.h"
	
#define LANGUAGE_ASSEMBLY
#include "internals.h"
#include "lispregs.h"

#if defined __FreeBSD__
#include <osreldate.h>
#endif

/* Minimize conditionalization for different OS naming schemes */
#if defined (__linux__) || (__FreeBSD_version >= 300000) || defined (__NetBSD__)
#define GNAME(var) var
#else
#define GNAME(var) _##var
#endif

/* Get the right type of alignment.  Linux wants alignment in bytes. */
#if defined (__linux__) || (__FreeBSD_version >= 300000)
#define align_4byte   4
#define       align_8byte     8
#define       align_16byte    16
#else
#define	align_4byte	2
#define	align_8byte	3
#define	align_16byte	4	
#endif			

	.text
	.global	GNAME(foreign_function_call_active)
	

/*
 * The C function will preserve rbx, r12-15, and rbp across its
 * function call - rbx is used to save the return lisp address.
 *
 * Return values are in rax, or st(0) for
 * floats.
 *
 * It should work for lisp calls C calls lisp calls C ..
 */
	.text
	.align	align_16byte,0x90
	.global GNAME(call_into_c)
	.type	GNAME(call_into_c),@function
GNAME(call_into_c):
	movl	$1,GNAME(foreign_function_call_active)

/* Save the return lisp address in rbx */	
	popq	%rbx

/* Setup the NPX for C */
	fstp	%st(0)
	fstp	%st(0)
	fstp	%st(0)
	fstp	%st(0)
	fstp	%st(0)
	fstp	%st(0)
	fstp	%st(0)
	fstp	%st(0)

	call	*%rax		# normal callout using Lisp stack

	movq	%rax,%rcx	# remember integer return value

/* Check for a return FP value */
	fxam
	fnstsw	%eax
	andl	$0x4500,%eax
	cmpl	$0x4100,%eax
	jne	Lfp_rtn_value

/* The return value is in eax, or eax,edx? */
/* Setup the NPX stack for lisp */
	fldz			# insure no regs are empty
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz

/* Restore the return value */
	movq	%rcx,%rax	# maybe return value

	movl	$0,GNAME(foreign_function_call_active)
/* Return */	
	jmp	*%rbx

Lfp_rtn_value:
/* The return result is in st(0) */
/* Setup the NPX stack for lisp, placing the result in st(0) */
	fldz			# insure no regs are empty
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz
	fxch	%st(7)		# move the result back to st(0)

/* Don't need to restore eax as the result is in st(0) */

	movl	$0,GNAME(foreign_function_call_active)
/* Return */	
	jmp	*%ebx

	.size	GNAME(call_into_c), . - GNAME(call_into_c)



	.text	
	.global GNAME(call_into_lisp)
	.type  GNAME(call_into_lisp),@function
		
/* The C conventions require that rbx, r12-r15, and rbp be preserved
	across function calls. */
/* The *alien-stack* pointer is setup on the first call_into_lisp when
   the stack changes. */
	
	.align	align_16byte,0x90
GNAME(call_into_lisp):
	pushq	%rbp		# save old frame pointer
	movq	%rsp,%rbp	# establish new frame

/* Save the NPX state */
	fwait			# Catch any pending NPX exceptions.
	subq	$108,%rsp	# Make room for the NPX state.
	fnsave	(%rsp)		# Resets NPX

	movl	(%rsp),%eax	# Load NPX control word
	andl	$0xfffff3ff,%eax	# Set rounding mode to nearest
#ifdef type_LongFloat
	orl	$0x00000300,%eax	# Set precision to 64 bits
#else
	orl	$0x00000200,%eax	# Set precision to 53 bits
#endif
	pushq	%rax
	fldcw	(%rsp)		# Recover modes
	popq	%rax

	fldz			# insure no FP regs are empty
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz
	
/* Save C regs: rbx r12-15 */
	pushq	%rbx
	pushq	%r12
	pushq	%r13
	pushq	%r14
	pushq	%r15
	
/* clear descriptor regs */
	xorq	%rax,%rax	# lexenv
	xorq	%rbx,%rbx	# available
	xorq	%rcx,%rcx	# arg count

/* no longer in function call */
	movl	$0, GNAME(foreign_function_call_active)

	movq	%rsp,%rbx	# remember current stack
	cmpq	$CONTROL_STACK_START,%rsp
	jbe	ChangeToLispStack
	cmpq	$CONTROL_STACK_END,%rsp
	jbe	OnLispStack
ChangeToLispStack:
	/* Setup the *alien-stack* pointer */
	movq	%rsp,ALIEN_STACK + SYMBOL_VALUE_OFFSET
	movq	$CONTROL_STACK_END,%rsp		# New stack
OnLispStack:
	pushq	%rbx		# save entry stack on (maybe) new stack

	/* establish lisp args */
	movq	%rdi,%rax	# lexenv? (C arg 1)
	xorq	%rdi,%rdi	# clear second arg (lisp)
	
	movq	%rsi,%rbx	# address of arg vec (C arg 2)
	xorq	%rsi,%rsi	# clear third arg (lisp)
	
	movq	%rdx,%rcx	# num args (C arg 3)
	xorq	%rdx,%rdx	# clear first arg (lisp)

	shlq	$2,%rcx		# make into fixnum
	cmpq	$0,%rcx
	je	Ldone
	movq	(%rbx),%rdx	# arg0
	cmpq	$4,%rcx
	je	Ldone
	movq	8(%rbx),%rdi	# arg1
	cmpq	$8,%rcx
	je	Ldone
	movq	16(%rbx),%rsi	# arg2
Ldone:	
	/* Registers rax (lexenv), rcx (num args), rdx (arg 0),rdi (arg 1),rsi (arg 2) now live */

	/* Allocate new frame */
	mov	%rsp,%rbx	# current sp marks start of new frame
	push	%rbp		# fp in save location S0
	sub	$16,%rsp	# Ensure 3 slots are allocated, one above.
	mov	%rbx,%rbp	# switch to new frame

	/* Indirect the closure */
	call	*CLOSURE_FUNCTION_OFFSET(%rax)
	
	/* Multi-value return - blow off any extra values */
	mov	%rbx, %rsp
	/* Single value return */	

/* Restore the stack, in case there was a stack change. */
	popq	%rsp		# c-sp

/* Restore C regs: rbx esi edi */
	popq	%r15
	popq	%r14
	popq	%r13
	popq	%r12
	popq	%rbx

/* Restore the NPX state */
	frstor  (%rsp)
	addq	$108, %rsp
	
	popq	%rbp		# c-fp
	movq	%rdx,%rax	# c-val
	ret
	.size	GNAME(call_into_lisp), . - GNAME(call_into_lisp)

/* Support for saving and restoring the NPX state from C. */
	.text
	.global	GNAME(fpu_save)
	.type	GNAME(fpu_save),@function
	.align	2,0x90
GNAME(fpu_save):
	fnsave	(%rdi)		# Save the NPX state - Resets NPX
	ret
	.size	GNAME(fpu_save),.-GNAME(fpu_save)

	.global	GNAME(fpu_restore)
	.type	GNAME(fpu_restore),@function
	.align	2,0x90
GNAME(fpu_restore):
	frstor	(%rdi)		# Restore the NPX state.
	ret
	.size	GNAME(fpu_restore),.-GNAME(fpu_restore)


/*
 * The undefined-function trampoline.
 */
	.text
	.align	align_4byte,0x90
	.global GNAME(undefined_tramp)
	.type	GNAME(undefined_tramp),@function
GNAME(undefined_tramp):
	int3
	.byte	trap_Error
        /* Number of argument bytes */
        .byte   2
	.byte	UNDEFINED_SYMBOL_ERROR
        /* SC_OFFSET(sc_DescriptorReg,reg_RAX) */
        .byte   SC_OFFSET(sc_DescriptorReg,0)
	ret
	.size	GNAME(undefined_tramp), .-GNAME(undefined_tramp)

/*
 * The closure trampoline.
 */
	.text
	.align	align_4byte,0x90
	.global GNAME(closure_tramp)
	.type	GNAME(closure_tramp),@function
GNAME(closure_tramp):
	movq	FDEFN_FUNCTION_OFFSET(%rax),%rax
	jmp	*CLOSURE_FUNCTION_OFFSET(%rax)
	.size	GNAME(closure_tramp), .-GNAME(closure_tramp)

/*
 * Function-end breakpoint magic.
 */
	.text
	.global	GNAME(function_end_breakpoint_guts)
	.align	align_4byte
GNAME(function_end_breakpoint_guts):
	/* Multiple Value return */
	jmp	multiple_value_return
	/* Single value return: The eventual return will now use the
	   multiple values return convention but with a return values
	   count of one. */
	movl	%esp,%ebx	# Setup ebx - the ofp.
	subl	$4,%esp		# Allocate one stack slot for the return value
	movl	$4,%ecx		# Setup ecx for one return value.
	movl	$NIL,%edi	# Default second value
	movl	$NIL,%esi	# Default third value
		
multiple_value_return:
	
	.global GNAME(function_end_breakpoint_trap)
GNAME(function_end_breakpoint_trap):
	int3
	.byte 	trap_FunctionEndBreakpoint
	hlt			# Should never return here.

	.global GNAME(function_end_breakpoint_end)
GNAME(function_end_breakpoint_end):


	.global	GNAME(do_pending_interrupt)
	.type	GNAME(do_pending_interrupt),@function
	.align	align_4byte,0x90
GNAME(do_pending_interrupt):
	int3
	.byte 	trap_PendingInterrupt
	ret
	.size	GNAME(do_pending_interrupt),.-GNAME(do_pending_interrupt)
	
#ifdef trap_DynamicSpaceOverflowError
	.global	GNAME(do_dynamic_space_overflow_error)
	.type	GNAME(do_dynamic_space_overflow_error),@function
	.align	align_4byte,0x90
GNAME(do_dynamic_space_overflow_error):
	int3
	.byte 	trap_DynamicSpaceOverflowError
	ret
	.size	GNAME(do_dynamic_space_overflow_error),.-GNAME(do_dynamic_space_overflow_error)
#endif

#ifdef trap_DynamicSpaceOverflowWarning
	.global	GNAME(do_dynamic_space_overflow_warning)
	.type	GNAME(do_dynamic_space_overflow_warning),@function
	.align	align_4byte,0x90
GNAME(do_dynamic_space_overflow_warning):
	int3
	.byte 	trap_DynamicSpaceOverflowWarning
	ret
	.size	GNAME(do_dynamic_space_overflow_warning),.-GNAME(do_dynamic_space_overflow_warning)
#endif


/*
   Allocate bytes and return the start of the allocated space
   in the specified destination register.

   In the general case the size will be in the destination register.

   All registers must be preserved except the destination.
   The C conventions will preserve rbx, r12-r15, and rbp.
   Linkage table will trash r11.
   So only rax, rdi, rsi, rdx, rcx, and r8-r10 need special care here. */
		
	.globl	GNAME(alloc_to_rax)
	.type	GNAME(alloc_to_rax),@function
	.align	align_4byte,0x90
GNAME(alloc_to_rax):
	pushq	%rcx	# Save rcx, rdx, rdi, rsi, r8-r10 as C could destroy them.
	pushq	%rdx
	pushq	%rdi
	pushq	%rsi
	pushq	%r8
	pushq	%r9
	pushq	%r10
	movq	%rax, %rdi	# Pass arg 1
	call	GNAME(alloc)	# return value is in %rax
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rsi
	popq	%rdi
	popq	%rdx	# Restore rcx and rdx.
	popq	%rcx
	ret
	.size	GNAME(alloc_to_rax),.-GNAME(alloc_to_rax)

	.globl	GNAME(alloc_to_rcx)
	.type	GNAME(alloc_to_rcx),@function
	.align	align_4byte,0x90
GNAME(alloc_to_rcx):
	pushq	%rax	# Save rax, rdi, rsi, rdx, and r8-r10 as C could destroy them.
	pushq	%rdi
	pushq	%rsi
	pushq	%rdx
	pushq	%r8
	pushq	%r9
	pushq	%r10
	movq	%rcx, %rdi	# Pass the size
	call	GNAME(alloc)
	movq	%rax, %rcx	# setup the destination.
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rdx	# Restore
	popq	%rsi
	popq	%rdi
	popq	%rax
	ret
	.size	GNAME(alloc_to_rcx),.-GNAME(alloc_to_rcx)

	.globl	GNAME(alloc_to_rdx)
	.type	GNAME(alloc_to_rdx),@function
	.align	align_4byte,0x90
GNAME(alloc_to_rdx):
	pushq	%rax	# Save rax, rcx, rdi, rsi, r8-r10 as C could destroy them.
	pushq	%rcx
	pushq	%rdi
	pushq	%rsi
	pushq	%r8
	pushq	%r9
	pushq	%r10
	mov	%rdx, %rdi	# move the size to arg 1
	call	GNAME(alloc)
	movq	%rax, %rdx	# setup the destination.
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rsi
	popq	%rdi	# Restore rdi, rax, and rcx.
	popq	%rcx	
	popq	%rax
	ret
	.size	GNAME(alloc_to_rdx),.-GNAME(alloc_to_rdx)

	.globl	GNAME(alloc_to_rbx)
	.type	GNAME(alloc_to_rbx),@function
	.align	align_4byte,0x90
GNAME(alloc_to_rbx):
	pushq	%rax	# Save rax, rcx, rdx, rdi, rsi, r8-r10 as C could destroy them.
	pushq	%rcx
	pushq	%rdx
	pushq	%rdi
	pushq	%rsi
	pushq	%r8
	pushq	%r9
	pushq	%r10
	movq	%rbx, %rdi	# Pass the size
	call	GNAME(alloc)
	movq	%rax, %rbx	# setup the destination.
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rsi
	popq	%rdi	
	popq	%rdx	# Restore rax, rcx, rdx, rdi, rsi, r8-r10.
	popq	%rcx
	popq	%rax
	ret
	.size	GNAME(alloc_to_rbx),.-GNAME(alloc_to_rbx)

	.globl	GNAME(alloc_to_rsi)
	.type	GNAME(alloc_to_rsi),@function
	.align	align_4byte,0x90
GNAME(alloc_to_rsi):
	pushq	%rax	# Save rax, rcx, rdx, rdi, r8-r10 as C could destroy them.
	pushq	%rcx
	pushq	%rdx
	pushq	%rdi
	pushq	%r8
	pushq	%r9
	pushq	%r10
	mov	%rsi, %rdi	# move the size to arg 1
	call	GNAME(alloc)
	movq	%rax,%rsi	# setup the destination.
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rdi	# Restore rdi, rax, rcx and rdx.
	popq	%rdx
	popq	%rcx
	popq	%rax
	ret
	.size	GNAME(alloc_to_rsi),.-GNAME(alloc_to_rsi)

	.globl	GNAME(alloc_to_rdi)
	.type	GNAME(alloc_to_rdi),@function
	.align	align_4byte,0x90
GNAME(alloc_to_rdi):
	pushq	%rax	# Save rax, rcx, rdx, rsi, r8-r10 as C could destroy them.
	pushq	%rcx
	pushq	%rdx
	pushq	%rsi
	pushq	%r8
	pushq	%r9
	pushq	%r10
	call	GNAME(alloc)
	movq	%rax, %rdi	# setup the destination.
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rsi
	popq	%rdx	# Restore rax, rcx and rdx.
	popq	%rcx
	popq	%rax
	ret
	.size	GNAME(alloc_to_rdi),.-GNAME(alloc_to_rdi)
		
	.globl	GNAME(alloc_to_r8)
	.type	GNAME(alloc_to_r8),@function
	.align	align_4byte,0x90
GNAME(alloc_to_r8):
	pushq	%rax	# Save rax, rcx, rdx, rdi, rsi, r9, r10 as C could destroy them.
	pushq	%rcx
	pushq	%rdx
	pushq	%rdi
	pushq	%rsi
	pushq	%r9
	pushq	%r10
	movq	%r8, %rdi	# Pass the size
	call	GNAME(alloc)
	movq	%rax, %r8	# setup the destination.
	popq	%r10
	popq	%r9
	popq	%rsi
	popq	%rdi
	popq	%rdx	# Restore rax, rcx and rdx.
	popq	%rcx
	popq	%rax
	ret
	.size	GNAME(alloc_to_r8),.-GNAME(alloc_to_r8)
	
	.globl	GNAME(alloc_to_r9)
	.type	GNAME(alloc_to_r9),@function
	.align	align_4byte,0x90
GNAME(alloc_to_r9):
	pushq	%rax	# Save rax, rcx, rdx, rdi, rsi, r8, r10 as C could destroy them.
	pushq	%rcx
	pushq	%rdx
	pushq	%rdi
	pushq	%rsi
	pushq	%r8
	pushq	%r10
	movq	%r9, %rdi	# Pass the size
	call	GNAME(alloc)
	movq	%rax, %r9	# setup the destination.
	popq	%r10
	popq	%r8
	popq	%rsi
	popq	%rdi
	popq	%rdx	# Restore rax, rcx and rdx.
	popq	%rcx
	popq	%rax
	ret
	.size	GNAME(alloc_to_r9),.-GNAME(alloc_to_r9)
	
	.globl	GNAME(alloc_to_r10)
	.type	GNAME(alloc_to_r10),@function
	.align	align_4byte,0x90
GNAME(alloc_to_r10):
	pushq	%rax	# Save rax, rcx, rdx, rdi, rsi, r8-r9 as C could destroy them.
	pushq	%rcx
	pushq	%rdx
	pushq	%rdi
	pushq	%rsi
	pushq	%r8
	pushq	%r9
	movq	%r10, %rdi	# Pass the size
	call	GNAME(alloc)
	movq	%rax, %r10	# setup the destination.
	popq	%r9
	popq	%r8
	popq	%rsi
	popq	%rdi
	popq	%rdx	# Restore rax, rcx and rdx.
	popq	%rcx
	popq	%rax
	ret
	.size	GNAME(alloc_to_r10),.-GNAME(alloc_to_r10)
	
	.globl	GNAME(alloc_to_r12)
	.type	GNAME(alloc_to_r12),@function
	.align	align_4byte,0x90
GNAME(alloc_to_r12):
	pushq	%rax	# Save rax, rcx, rdx, rdi, rsi, r8-r10 as C could destroy them.
	pushq	%rcx
	pushq	%rdx
	pushq	%rdi
	pushq	%rsi
	pushq	%r8
	pushq	%r9
	pushq	%r10
	movq	%r12, %rdi	# Pass the size
	call	GNAME(alloc)
	movq	%rax, %r12	# setup the destination.
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rsi
	popq	%rdi
	popq	%rdx	# Restore rax, rcx and rdx.
	popq	%rcx
	popq	%rax
	ret
	.size	GNAME(alloc_to_r12),.-GNAME(alloc_to_r12)

	.globl	GNAME(alloc_to_r13)
	.type	GNAME(alloc_to_r13),@function
	.align	align_4byte,0x90
GNAME(alloc_to_r13):
	pushq	%rax	# Save rax, rcx, rdx, rdi, rsi, r8-r10 as C could destroy them.
	pushq	%rcx
	pushq	%rdx
	pushq	%rdi
	pushq	%rsi
	pushq	%r8
	pushq	%r9
	pushq	%r10
	movq	%r13, %rdi	# Pass the size
	call	GNAME(alloc)
	movq	%rax, %r13	# setup the destination.
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rsi
	popq	%rdi
	popq	%rdx	# Restore rax, rcx and rdx.
	popq	%rcx
	popq	%rax
	ret
	.size	GNAME(alloc_to_r13),.-GNAME(alloc_to_r13)


#ifdef GENCGC

/* Called from lisp when an inline allocation overflows.
   Every register except the result needs to be preserved.
   We depend on C to preserve rbx, r12-r15, and rbp.
   Linkage table will trash r11.
   But where necessary must save rax, rcx, rdx, rdi, rsi, r8-r10. */

/* This routine handles an overflow with rax=crfp+size. So the
   size=rax-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_rax)
	.type	GNAME(alloc_overflow_rax),@function
GNAME(alloc_overflow_rax):
	pushq	%rcx		# Save rcx
	pushq	%rdx		# Save rdx
	pushq	%rdi
	pushq	%rsi
	pushq	%r8
	pushq	%r9
	pushq	%r10
	/* Calculate the size for the allocation. */
	subq	CURRENT_REGION_FREE_POINTER + SYMBOL_VALUE_OFFSET,%rax
	movq	%rax, %rdi	# Pass arg 1
	call	GNAME(alloc)
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rsi
	popq	%rdi
	popq	%rdx	# Restore rdx.
	popq	%rcx	# Restore rcx.
	addq	$13,(%rsp) # Adjust the return address to skip the next inst.
	ret
	.size	 GNAME(alloc_overflow_rax),.-GNAME(alloc_overflow_rax)

/* This routine handles an overflow with rcx=crfp+size. So the
   size=rcx-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_rcx)
	.type	GNAME(alloc_overflow_rcx),@function
GNAME(alloc_overflow_rcx):
	pushq	%rax		# Save rax
	pushq	%rdx		# Save rdx
	pushq	%rdi
	pushq	%rsi
	pushq	%r8
	pushq	%r9
	pushq	%r10
	/* Calculate the size for the allocation. */
	subq	CURRENT_REGION_FREE_POINTER + SYMBOL_VALUE_OFFSET,%rcx
	movq	%rcx, %rdi	# Pass arg1
	call	GNAME(alloc)
	movq	%rax, %rcx	# setup the destination.
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rsi
	popq	%rdi
	popq	%rdx	# Restore rdx.
	popq	%rax	# Restore rax.
	addq	$13,(%rsp) # Adjust the return address to skip the next 2 inst.
	ret
	.size	 GNAME(alloc_overflow_rcx),.-GNAME(alloc_overflow_rcx)

/* This routine handles an overflow with rdx=crfp+size. So the
   size=rdx-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_rdx)
	.type	GNAME(alloc_overflow_rdx),@function
GNAME(alloc_overflow_rdx):
	pushq	%rax		# Save rax
	pushq	%rcx		# Save rcx
	pushq	%rdi
	pushq	%rsi
	pushq	%r8
	pushq	%r9
	pushq	%r10
	/* Calculate the size for the allocation. */
	subq	CURRENT_REGION_FREE_POINTER + SYMBOL_VALUE_OFFSET,%rdx
	movq	%rdx, %rdi		# Move the size
	call	GNAME(alloc)
	movq	%rax,%rdx	# setup the destination.
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rsi
	popq	%rdi
	popq	%rcx	# Restore rcx.
	popq	%rax	# Restore rax.
	addq	$13,(%rsp) # Adjust the return address to skip the next 2 inst.
			# The next 2 instructions sets the CRFP.
	ret
	.size	 GNAME(alloc_overflow_rdx),.-GNAME(alloc_overflow_rdx)

/* This routine handles an overflow with rbx=crfp+size. So the
   size=rbx-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_rbx)
	.type	GNAME(alloc_overflow_rbx),@function
GNAME(alloc_overflow_rbx):
	pushq	%rax		# Save rax
	pushq	%rcx		# Save rcx
	pushq	%rdx		# Save rdx
	pushq	%rdi
	pushq	%rsi
	pushq	%r8
	pushq	%r9
	pushq	%r10
	/* Calculate the size for the allocation. */
	subq	CURRENT_REGION_FREE_POINTER + SYMBOL_VALUE_OFFSET,%rbx
	movq	%rbx, %rdi	# Pass arg 1
	call	GNAME(alloc)
	movq	%rax,%rbx	# setup the destination.
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rsi
	popq	%rdi
	popq	%rdx	# Restore rdx.
	popq	%rcx	# Restore rcx.
	popq	%rax	# Restore rax.
	addq	$13,(%rsp) # Adjust the return address to skip the next 2 inst.
	ret
	.size	 GNAME(alloc_overflow_rbx),.-GNAME(alloc_overflow_rbx)

/* This routine handles an overflow with rsi=crfp+size. So the
   size=rsi-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_rsi)
	.type	GNAME(alloc_overflow_rsi),@function
GNAME(alloc_overflow_rsi):
	pushq	%rax		# Save rax
	pushq	%rcx		# Save rcx
	pushq	%rdx		# Save rdx
	pushq	%rdi
	pushq	%r8
	pushq	%r9
	pushq	%r10
	/* Calculate the size for the allocation. */
	subq	CURRENT_REGION_FREE_POINTER + SYMBOL_VALUE_OFFSET,%rsi
	movq	%rsi, %rdi	# Pass arg 1
	call	GNAME(alloc)
	movq	%rax, %rsi	# setup the destination.
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rdi
	popq	%rdx	# Restore rdx.
	popq	%rcx	# Restore rcx.
	popq	%rax	# Restore rax.
	addq	$13,(%rsp) # Adjust the return address to skip the next 2 inst.
	ret
	.size	 GNAME(alloc_overflow_rsi),.-GNAME(alloc_overflow_rsi)

/* This routine handles an overflow with rdi=crfp+size. So the
   size=rdi-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_rdi)
	.type	GNAME(alloc_overflow_rdi),@function
GNAME(alloc_overflow_rdi):
	pushq	%rax		# Save rax
	pushq	%rcx		# Save rcx
	pushq	%rdx		# Save rdx
	pushq	%rsi
	pushq	%r8
	pushq	%r9
	pushq	%r10
	/* Calculate the size for the allocation. */
	subq	CURRENT_REGION_FREE_POINTER + SYMBOL_VALUE_OFFSET,%rdi
	call	GNAME(alloc)
	movq	%rax,%rdi	# setup the destination.
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rsi
	popq	%rdx	# Restore rdx.
	popq	%rcx	# Restore rcx.
	popq	%rax	# Restore rax.
	addl	$13,(%rsp) # Adjust the return address to skip the next inst.
	ret
	.size	 GNAME(alloc_overflow_rdi),.-GNAME(alloc_overflow_rdi)

/* This routine handles an overflow with r8=crfp+size. So the
   size=r8-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_r8)
	.type	GNAME(alloc_overflow_r8),@function
GNAME(alloc_overflow_r8):
	pushq	%rax		# Save rax
	pushq	%rcx		# Save rcx
	pushq	%rdx		# Save rdx
	pushq	%rdi
	pushq	%rsi
	pushq	%r9
	pushq	%r10
	/* Calculate the size for the allocation. */
	subq	CURRENT_REGION_FREE_POINTER + SYMBOL_VALUE_OFFSET,%r8
	movq	%r8, %rdi	# Pass arg 1
	call	GNAME(alloc)
	movq	%rax,%r8	# setup the destination.
	popq	%r10
	popq	%r9
	popq	%rsi
	popq	%rdi
	popq	%rdx	# Restore rdx.
	popq	%rcx	# Restore rcx.
	popq	%rax	# Restore rax.
	addl	$13,(%rsp) # Adjust the return address to skip the next inst.
	ret
	.size	 GNAME(alloc_overflow_r8),.-GNAME(alloc_overflow_r8)

/* This routine handles an overflow with r9=crfp+size. So the
   size=r9-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_r9)
	.type	GNAME(alloc_overflow_r9),@function
GNAME(alloc_overflow_r9):
	pushq	%rax		# Save rax
	pushq	%rcx		# Save rcx
	pushq	%rdx		# Save rdx
	pushq	%rdi
	pushq	%rsi
	pushq	%r8
	pushq	%r10
	/* Calculate the size for the allocation. */
	subq	CURRENT_REGION_FREE_POINTER + SYMBOL_VALUE_OFFSET,%r9
	movq	%r9, %rdi	# Pass arg 1
	call	GNAME(alloc)
	movq	%rax,%r9	# setup the destination.
	popq	%r10
	popq	%r8
	popq	%rsi
	popq	%rdi
	popq	%rdx	# Restore rdx.
	popq	%rcx	# Restore rcx.
	popq	%rax	# Restore rax.
	addl	$13,(%rsp) # Adjust the return address to skip the next inst.
	ret
	.size	 GNAME(alloc_overflow_r9),.-GNAME(alloc_overflow_r9)

/* This routine handles an overflow with r10=crfp+size. So the
   size=r10-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_r10)
	.type	GNAME(alloc_overflow_r10),@function
GNAME(alloc_overflow_r10):
	pushq	%rax		# Save rax
	pushq	%rcx		# Save rcx
	pushq	%rdx		# Save rdx
	pushq	%rdi
	pushq	%rsi
	pushq	%r8
	pushq	%r9
	/* Calculate the size for the allocation. */
	subq	CURRENT_REGION_FREE_POINTER + SYMBOL_VALUE_OFFSET,%r10
	movq	%r10, %rdi	# Pass arg 1
	call	GNAME(alloc)
	movq	%rax,%r10	# setup the destination.
	popq	%r9
	popq	%r8
	popq	%rsi
	popq	%rdi
	popq	%rdx	# Restore rdx.
	popq	%rcx	# Restore rcx.
	popq	%rax	# Restore rax.
	addl	$13,(%rsp) # Adjust the return address to skip the next inst.
	ret
	.size	 GNAME(alloc_overflow_r10),.-GNAME(alloc_overflow_r10)

/* r11 is used by linkage table */

/* This routine handles an overflow with r12=crfp+size. So the
   size=r12-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_r12)
	.type	GNAME(alloc_overflow_r12),@function
GNAME(alloc_overflow_r12):
	pushq	%rax		# Save rax
	pushq	%rcx		# Save rcx
	pushq	%rdx		# Save rdx
	pushq	%rdi
	pushq	%rsi
	pushq	%r8
	pushq	%r9
	pushq	%r10
	/* Calculate the size for the allocation. */
	subq	CURRENT_REGION_FREE_POINTER + SYMBOL_VALUE_OFFSET,%r12
	movq	%r12, %rdi	# Pass arg 1
	call	GNAME(alloc)
	movq	%rax,%r12	# setup the destination.
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rsi
	popq	%rdi
	popq	%rdx	# Restore rdx.
	popq	%rcx	# Restore rcx.
	popq	%rax	# Restore rax.
	addl	$13,(%rsp) # Adjust the return address to skip the next inst.
	ret
	.size	 GNAME(alloc_overflow_r12),.-GNAME(alloc_overflow_r12)

/* This routine handles an overflow with r13=crfp+size. So the
   size=r13-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_r13)
	.type	GNAME(alloc_overflow_r13),@function
GNAME(alloc_overflow_r13):
	pushq	%rax		# Save rax
	pushq	%rcx		# Save rcx
	pushq	%rdx		# Save rdx
	pushq	%rdi
	pushq	%rsi
	pushq	%r8
	pushq	%r9
	pushq	%r10
	/* Calculate the size for the allocation. */
	subq	CURRENT_REGION_FREE_POINTER + SYMBOL_VALUE_OFFSET,%r13
	movq	%r13, %rdi	# Pass arg 1
	call	GNAME(alloc)
	movq	%rax,%r13	# setup the destination.
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rsi
	popq	%rdi
	popq	%rdx	# Restore rdx.
	popq	%rcx	# Restore rcx.
	popq	%rax	# Restore rax.
	addl	$13,(%rsp) # Adjust the return address to skip the next inst.
	ret
	.size	 GNAME(alloc_overflow_r13),.-GNAME(alloc_overflow_r13)
			
#endif

#ifdef LINKAGE_TABLE

/* Call into C code to resolve a linkage entry.  The initial code in the 
 * linkage entry has done a call to here; pass that return entry along as a 
 * parameter.
 *
 * We could be called from raw Lisp code or from a foreign call site, so we 
 * have to save all the registers...
 */	
	.align	align_4byte
	.globl	GNAME(resolve_linkage_tramp)
	.type	GNAME(resolve_linkage_tramp),@function
GNAME(resolve_linkage_tramp):
	pushq	%rbp		# save old frame pointer
	movq	%rsp,%rbp	# establish new frame
	pushq	%rax
	# rbx is callee saved
	pushq	%rcx
	pushq	%rdx
	pushq	%rdi
	pushq	%rsi
	pushq	%r8
	pushq	%r9
	pushq	%r10
	pushq	%r11
	# r12-15 is callee saved
		
	/* calling location (plus offset) was on top of stack */
	movq	8(%rbp), %rdi	# arg 1 for C function
	call	GNAME(lazy_resolve_linkage)
	/* real address of target is in %rax.  Replace return address on stack
	 * with it.  That way we can get out of here without trashing any 
	 * registers!
	 */
	movq	%rax,8(%rbp)
	popq	%r11
	popq	%r10
	popq	%r9
	popq	%r8
	popq	%rsi
	popq	%rdi
	popq	%rdx
	popq	%rcx
	popq	%rax
	popq	%rbp
	ret			# jump to the real target
	.size	 GNAME(resolve_linkage_tramp),.-GNAME(resolve_linkage_tramp)

/*
 * The C-callable undefined-foreign-symbol trapping function.
 */
	.text
	.align	align_4byte,0x90
	.global GNAME(undefined_foreign_symbol_trap)
	.type	GNAME(undefined_foreign_symbol_trap),@function
GNAME(undefined_foreign_symbol_trap):
	/* C Calling Convention, move one arg to RAX */
        pushq %rbp
        movq %rsp,%rbp
        movq %rdi,%rax

	/* Now trap to Lisp */
	int3
	.byte	trap_Error
        /* Number of argument bytes */
        .byte   2
	.byte	UNDEFINED_FOREIGN_SYMBOL_ERROR
        /* SC_OFFSET(sc_DescriptorReg,reg_RAX) */
        .byte   SC_OFFSET(sc_DescriptorReg,0)

	/* C Calling Convention */
	/* Doesn't matter here, but could if we'd use trap_Cerror */
	leave
	ret
	.size	GNAME(undefined_foreign_symbol_trap), .-GNAME(undefined_foreign_symbol_trap)
	.end

#endif /* LINKAGE_TABLE */
