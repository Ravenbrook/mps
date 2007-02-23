#define LANGUAGE_ASSEMBLY

#include "internals.h"
#include "lispregs.h"

	.import $global$,data
	.import foreign_function_call_active,data
	.import current_control_stack_pointer,data
	.import current_control_frame_pointer,data
	.import current_binding_stack_pointer,data
	.import current_dynamic_space_free_pointer,data

	.space	$TEXT$
	.subspa	$CODE$
	.import $$dyncall,MILLICODE


/*
 * Call-into-lisp
 */

	.export call_into_lisp
call_into_lisp
	.proc
	.callinfo entry_gr=18,save_rp
	.enter
	/* arg0=function, arg1=cfp, arg2=nargs */

	/* Clear the descriptor regs, moving in args as approporate. */
	copy	r0,reg_CODE
	copy	r0,reg_FDEFN
	copy	arg0,reg_LEXENV
	zdep	arg2,29,30,reg_NARGS
	copy	r0,reg_OCFP
	copy	r0,reg_LRA
	copy	r0,reg_A0
	copy	r0,reg_A1
	copy	r0,reg_A2
	copy	r0,reg_A3
	copy	r0,reg_A4
	copy	r0,reg_A5
	copy	r0,reg_L0
	copy	r0,reg_L1
	copy	r0,reg_L2

	/* Establish NIL. */
	ldil	L%NIL,reg_NULL
	ldo	R%NIL(reg_NULL),reg_NULL

	/* Turn on pseudo-atomic. */
	ldo	4(r0),reg_ALLOC

	/* No longer in foreign function call land. */
	addil	L%foreign_function_call_active-$global$,dp
	stw	r0,R%foreign_function_call_active-$global$(0,r1)

	/* Load lisp state. */
	addil	L%current_dynamic_space_free_pointer-$global$,dp
	ldw	R%current_dynamic_space_free_pointer-$global$(0,r1),r1
	add	reg_ALLOC,r1,reg_ALLOC
	addil	L%current_binding_stack_pointer-$global$,dp
	ldw	R%current_binding_stack_pointer-$global$(0,r1),reg_BSP
	addil	L%current_control_stack_pointer-$global$,dp
	ldw	R%current_control_stack_pointer-$global$(0,r1),reg_CSP
	addil	L%current_control_frame_pointer-$global$,dp
	ldw	R%current_control_frame_pointer-$global$(0,r1),reg_OCFP
	copy	arg1,reg_CFP

	/* End of pseudo-atomic. */
	addit,od	-4,reg_ALLOC,reg_ALLOC

	/* Establish lisp arguments. */
	ldw	0(reg_CFP),reg_A0
	ldw	4(reg_CFP),reg_A1
	ldw	8(reg_CFP),reg_A2
	ldw	12(reg_CFP),reg_A3
	ldw	16(reg_CFP),reg_A4
	ldw	20(reg_CFP),reg_A5

	/* Calculate the LRA. */
	ldil	L%lra+type_OtherPointer,reg_LRA
	ldo	R%lra+type_OtherPointer(reg_LRA),reg_LRA

	/* Indirect the closure */
	ldw	CLOSURE_FUNCTION_OFFSET(0,reg_LEXENV),reg_CODE
	addi	6*4-type_FunctionPointer,reg_CODE,reg_LIP

	/* And into lisp we go. */
	.export break_here
break_here
	be,n	0(sr5,reg_LIP)

	.align	8
lra
	.word	type_ReturnPcHeader
	copy	reg_OCFP,reg_CSP

	/* Copy CFP (r4) into someplace else and restore r4. */
	copy	reg_CFP,reg_NL1
	ldw	-64(0,sp),r4

	/* Copy the return value. */
	copy	reg_A0,ret0

	/* Turn on pseudo-atomic. */
	addi	4,reg_ALLOC,reg_ALLOC

	/* Store the lisp state. */
	copy	reg_ALLOC,reg_NL0
	depi	0,31,3,reg_NL0
	addil	L%current_dynamic_space_free_pointer-$global$,dp
	stw	reg_NL0,R%current_dynamic_space_free_pointer-$global$(0,r1)
	addil	L%current_binding_stack_pointer-$global$,dp
	stw	reg_BSP,R%current_binding_stack_pointer-$global$(0,r1)
	addil	L%current_control_stack_pointer-$global$,dp
	stw	reg_CSP,R%current_control_stack_pointer-$global$(0,r1)
	addil	L%current_control_frame_pointer-$global$,dp
	stw	reg_NL1,R%current_control_frame_pointer-$global$(0,r1)

	/* Back in C land.  [CSP is just a handy non-zero value.] */
	addil	L%foreign_function_call_active-$global$,dp
	stw	reg_CSP,R%foreign_function_call_active-$global$(0,r1)

	/* Turn off pseudo-atomic and check for traps. */
	addit,od	-4,reg_ALLOC,reg_ALLOC

	/* And thats all. */
	.leave
	.procend


/*
 * Call-into-C
 */

	
	.export call_into_c
call_into_c
	/* Set up a lisp stack frame.  Note: we convert the raw return pc into
	 * a fixnum pc-offset because we don't have ahold of an lra object.
	 */
	copy	reg_CFP, reg_OCFP
	copy	reg_CSP, reg_CFP
	addi	32, reg_CSP, reg_CSP
	stw	reg_OCFP, 0(0,reg_CFP)
	sub	reg_LIP, reg_CODE, reg_NL5
	addi	3-type_OtherPointer, reg_NL5, reg_NL5
	stw	reg_NL5, 4(0,reg_CFP)
	stw	reg_CODE, 8(0,reg_CFP)

	/* Turn on pseudo-atomic. */
	addi	4, reg_ALLOC, reg_ALLOC

	/* Store the lisp state. */
	copy	reg_ALLOC,reg_NL5
	depi	0,31,3,reg_NL5
	addil	L%current_dynamic_space_free_pointer-$global$,dp
	stw	reg_NL5,R%current_dynamic_space_free_pointer-$global$(0,r1)
	addil	L%current_binding_stack_pointer-$global$,dp
	stw	reg_BSP,R%current_binding_stack_pointer-$global$(0,r1)
	addil	L%current_control_stack_pointer-$global$,dp
	stw	reg_CSP,R%current_control_stack_pointer-$global$(0,r1)
	addil	L%current_control_frame_pointer-$global$,dp
	stw	reg_CFP,R%current_control_frame_pointer-$global$(0,r1)

	/* Back in C land.  [CSP is just a handy non-zero value.] */
	addil	L%foreign_function_call_active-$global$,dp
	stw	reg_CSP,R%foreign_function_call_active-$global$(0,r1)

	/* Turn off pseudo-atomic and check for traps. */
	addit,od	-4,reg_ALLOC,reg_ALLOC

	/* in order to be able to call incrementally linked (ld -A) functions,
	   we have to do some mild trickery here */
	copy  reg_CFUNC,%r22
	bl    $$dyncall,r31
	copy  r31, r2

	/* Clear the callee saves descriptor regs. */
	copy	r0, reg_A5
	copy	r0, reg_L0
	copy	r0, reg_L1
	copy	r0, reg_L2

	/* Turn on pseudo-atomic. */
	ldi	4, reg_ALLOC

	/* Turn off foreign function call. */
	addil	L%foreign_function_call_active-$global$,dp
	stw	r0,R%foreign_function_call_active-$global$(0,r1)

	/* Load ALLOC. */
	addil	L%current_dynamic_space_free_pointer-$global$,dp
	ldw	R%current_dynamic_space_free_pointer-$global$(0,r1),r1
	add	reg_ALLOC,r1,reg_ALLOC

	/* We don't need to load OCFP, CFP, CSP, or BSP because they are
	 * in caller saves registers.
	 */

	/* End of pseudo-atomic. */
	addit,od	-4,reg_ALLOC,reg_ALLOC

	/* Restore CODE.  Even though it is in a callee saves register
	 * it might have been GC'ed.
	 */
	ldw	8(0,reg_CFP), reg_CODE

	/* Restore the return pc. */
	ldw	4(0,reg_CFP), reg_NL0
	addi	type_OtherPointer-3, reg_NL0, reg_NL0
	add	reg_CODE, reg_NL0, reg_LIP

	/* Pop the lisp stack frame, and back we go. */
	copy	reg_CFP, reg_CSP
	be	0(4,reg_LIP)
	copy	reg_OCFP, reg_CFP



/*
 * Stuff to sanctify a block of memory for execution.
 */

	.EXPORT sanctify_for_execution
sanctify_for_execution
	.proc
	.callinfo
	.enter
	/* arg0=start addr, arg1=length in bytes */
	add	arg0,arg1,arg1
	ldo	-1(arg1),arg1
	depi	0,31,5,arg0
	depi	0,31,5,arg1
	ldsid	(arg0),r19
	mtsp	r19,sr1
	ldi	32,r19			; bytes per cache line
sanctify_loop
	fdc	0(sr1,arg0)
	comb,<	arg0,arg1,sanctify_loop
	fic,m	r19(sr1,arg0)
	.leave
	.procend


/*
 * Trampolines.
 */

	.EXPORT closure_tramp
closure_tramp
	/* reg_FDEFN holds the fdefn object. */
	ldw	FDEFN_FUNCTION_OFFSET(0,reg_FDEFN),reg_LEXENV
	ldw	CLOSURE_FUNCTION_OFFSET(0,reg_LEXENV),reg_L0
	addi	FUNCTION_CODE_OFFSET, reg_L0, reg_LIP
	bv,n	0(reg_LIP)

	.EXPORT undefined_tramp
undefined_tramp
	break	trap_Error,0
        /* Number of argument bytes */
        .byte	4
        .byte	UNDEFINED_SYMBOL_ERROR
        /* Escape to create 16bit BE number from following two values */
        .byte	254
        /* SC_OFFSET(sc_DescriptorReg,reg_LEXENV) */
	/* Shouldn't this be reg_FDEFN, instead? */
        .byte	(0x40 + sc_DescriptorReg)
        .byte	1
	.align	4


/*
 * Core saving/restoring support
 */

	.export call_on_stack
call_on_stack
	/* arg0 = fn to invoke, arg1 = new stack base */

	/* Compute the new stack pointer. */
	addi	64,arg1,sp

	/* Zero out the previous stack pointer. */
	stw	r0,-4(0,sp)

	/* Invoke the function. */
	ble	0(4,arg0)
	copy	r31, r2

	/* Flame out. */
	break	0,0

	.export	save_state
save_state
	.proc
	.callinfo entry_gr=18,entry_fr=21,save_rp,calls
	.enter

	/* Remember the function we want to invoke */
	copy	arg0,r19

	/* Pass the new stack pointer in as arg0 */
	copy	sp,arg0

	/* Leave arg1 as arg1. */

	/* do the call. */
	ble	0(4,r19)
	copy	r31, r2

_restore_state
	.leave
	.procend

	.export	restore_state
restore_state
	.proc
	.callinfo
	copy	arg0,sp
	b	_restore_state
	copy	arg1,ret0
	.procend



	.export SingleStepTraps
SingleStepTraps
	break	trap_SingleStepBreakpoint,0
	break	trap_SingleStepBreakpoint,0



	.align	8
	.export function_end_breakpoint_guts
function_end_breakpoint_guts
	.word	type_ReturnPcHeader
	/* multiple value return point -- just jump to trap. */
	b,n	function_end_breakpoint_trap
	/* single value return point -- convert to multiple w/ n=1 */
	copy	reg_CSP, reg_OCFP
	addi	4, reg_CSP, reg_CSP
	addi	4, r0, reg_NARGS
	copy	reg_NULL, reg_A1
	copy	reg_NULL, reg_A2
	copy	reg_NULL, reg_A3
	copy	reg_NULL, reg_A4
	copy	reg_NULL, reg_A5

	.export	function_end_breakpoint_trap
function_end_breakpoint_trap
	break	trap_FunctionEndBreakpoint,0
	b,n	function_end_breakpoint_trap

	.export	function_end_breakpoint_end
function_end_breakpoint_end
