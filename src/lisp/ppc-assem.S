#define LANGUAGE_ASSEMBLY

#include "internals.h"
#include "lispregs.h"
#include "globals.h"

#if defined DARWIN
#define FUNCDEF(x)	.text @ \
			.align 3 @ \
_/**/x:
#define GFUNCDEF(x)	.globl _/**/x @ \
	FUNCDEF(x)
#else
#define FUNCDEF(x)	.text ; \
			.align 3 ; \
			.type x,@function ; \
x:
#define GFUNCDEF(x)	.globl _/**/x @ \
	FUNCDEF(x)
#endif

#if defined DARWIN
#define SET_SIZE(x)
#else
#define SET_SIZE(x) .size x,.-x
#endif

/* Load a register from a global, using the register as an intermediary */
/* The register will be a fixnum for one instruction, so this is gc-safe */

#if defined DARWIN
#define load(reg,global) \
	lis reg,ha16(global) @ \
	lwz reg,lo16(global)(reg) ; Comment
#define store(reg,temp,global) \
	lis temp,ha16(global) @\
	stw reg,lo16(global)(temp) ; Comment
#else
#define load(reg,global) \
	lis reg,global@ha; lwz reg,global@l(reg)
#define store(reg,temp,global) \
	lis temp,global@ha; stw reg,global@l(temp)
#endif
	
#define	FIRST_SAVE_FPR	14	/* lowest-numbered non-volatile FPR */
#define	FIRST_SAVE_GPR	13	/* lowest-numbered non-volatile GPR */
#define	NFPR_SAVE_BYTES(n) ((32-(n))*8)
#if defined DARWIN
#define NGPR_SAVE_BYTES(n) ((32-(n))*4)
#define FRAME_ARG_BYTES(n)  (((((n)+6)*4)+15)&~15)
#else
#define NGPR_SAVE_BYTES(n) ((32-(~1&((n)+1)))*4)
#define FRAME_ARG_BYTES(n)  (((((n)+2)*4)+15)&~15)
#endif

#if defined DARWIN
#define FRAME_SIZE(first_g,first_f,out_arg_words,savecr) \
(NFPR_SAVE_BYTES(first_f)+ NGPR_SAVE_BYTES(first_g)+ FRAME_ARG_BYTES(out_arg_words))
#define SAVE_FPR(n) stfd f/**/n,-8*(32- n)(r11)
#define SAVE_GPR(n) stw r/**/n,-4*(32- n)(r11)
/* Make sure FULL_FRAME_SIZE is on a 16-byte boundary */
#define FULL_FRAME_SIZE ((FRAME_SIZE(FIRST_SAVE_GPR,FIRST_SAVE_FPR,8,1) + 15) & ~15)
#else
#define FRAME_SIZE(first_g,first_f,out_arg_words,savecr) \
(NFPR_SAVE_BYTES(first_f)+ NGPR_SAVE_BYTES(first_g)+ FRAME_ARG_BYTES(out_arg_words+savecr))
#define SAVE_FPR(n) stfd n,-8*(32-(n))(11)
#define SAVE_GPR(n) stw n,-4*(32-(n))(11)
#define FULL_FRAME_SIZE FRAME_SIZE(FIRST_SAVE_GPR,FIRST_SAVE_FPR,0,1)
#endif
	
#if defined DARWIN
#define RESTORE_FPR(n) lfd f/**/n,-8*(32- n)(r11)
#define RESTORE_GPR(n) lwz r/**/n,-4*(32- n)(r11)
#else
#define RESTORE_FPR(n) lfd f##n,-8*(32-(r##n))(r11)
#define RESTORE_GPR(n) lwz r##n,-4*(32-(r##n))(r11)
#endif
#define C_FULL_PROLOG \
	nop @\
	nop @ \
	mfcr REG(0) @ \
	stw REG(0),4(REG(1)) @ \
	mflr REG(0) @ \
	stw REG(0),8(REG(1)) @ \
	mr REG(11),REG(1) @ \
	stwu REG(1),-FULL_FRAME_SIZE(REG(1)) @ \
	SAVE_FPR(14) @ \
	SAVE_FPR(15) @ \
	SAVE_FPR(16) @ \
	SAVE_FPR(17) @ \
	SAVE_FPR(18) @ \
	SAVE_FPR(19) @ \
	SAVE_FPR(20) @ \
	SAVE_FPR(21) @ \
	SAVE_FPR(22) @ \
	SAVE_FPR(23) @ \
	SAVE_FPR(24) @ \
	SAVE_FPR(25) @ \
	SAVE_FPR(26) @ \
	SAVE_FPR(27) @ \
	SAVE_FPR(28) @ \
	SAVE_FPR(29) @ \
	SAVE_FPR(30) @ \
	SAVE_FPR(31) @ \
	la REG(11),-NFPR_SAVE_BYTES(FIRST_SAVE_FPR)(REG(11)) @ \
	SAVE_GPR(13) @ \
	SAVE_GPR(14) @ \
	SAVE_GPR(15) @ \
	SAVE_GPR(16) @ \
	SAVE_GPR(17) @ \
	SAVE_GPR(18) @ \
	SAVE_GPR(19) @ \
	SAVE_GPR(20) @ \
	SAVE_GPR(21) @ \
	SAVE_GPR(22) @ \
	SAVE_GPR(23) @ \
	SAVE_GPR(24) @ \
	SAVE_GPR(25) @ \
	SAVE_GPR(26) @ \
	SAVE_GPR(27) @ \
	SAVE_GPR(28) @ \
	SAVE_GPR(29) @ \
	SAVE_GPR(30) @ \
	SAVE_GPR(31)


#define C_FULL_EPILOG \
	la REG(11),FULL_FRAME_SIZE-NFPR_SAVE_BYTES(FIRST_SAVE_FPR)(REG(1)) @ \
	RESTORE_GPR(13) @ \
	RESTORE_GPR(14) @ \
	RESTORE_GPR(15) @ \
	RESTORE_GPR(16) @ \
	RESTORE_GPR(17) @ \
	RESTORE_GPR(18) @ \
	RESTORE_GPR(19) @ \
	RESTORE_GPR(20) @ \
	RESTORE_GPR(21) @ \
	RESTORE_GPR(22) @ \
	RESTORE_GPR(23) @ \
	RESTORE_GPR(24) @ \
	RESTORE_GPR(25) @ \
	RESTORE_GPR(26) @ \
	RESTORE_GPR(27) @ \
	RESTORE_GPR(28) @ \
	RESTORE_GPR(29) @ \
	RESTORE_GPR(30) @ \
	RESTORE_GPR(31) @ \
	la REG(11),NFPR_SAVE_BYTES(FIRST_SAVE_FPR)(REG(11)) @ \
	RESTORE_FPR(14) @ \
	RESTORE_FPR(15) @ \
	RESTORE_FPR(16) @ \
	RESTORE_FPR(17) @ \
	RESTORE_FPR(18) @ \
	RESTORE_FPR(19) @ \
	RESTORE_FPR(20) @ \
	RESTORE_FPR(21) @ \
	RESTORE_FPR(22) @ \
	RESTORE_FPR(23) @ \
	RESTORE_FPR(24) @ \
	RESTORE_FPR(25) @ \
	RESTORE_FPR(26) @ \
	RESTORE_FPR(27) @ \
	RESTORE_FPR(28) @ \
	RESTORE_FPR(29) @ \
	RESTORE_FPR(30) @ \
	RESTORE_FPR(31) @ \
	lwz REG(1),0(REG(1)) @ \
	lwz REG(0),4(REG(1)) @ \
	mtcr REG(0) @ \
	lwz REG(0),8(REG(1)) @ \
	mtlr REG(0) @ \
	


	
	.text

/*
 * Function to transfer control into lisp.  The lisp object to invoke is
 * passed as the first argument, which puts it in NL0
 */

	GFUNCDEF(call_into_lisp)
	C_FULL_PROLOG
	/* store(reg_POLL,11,saver2) */
	/* Initialize tagged registers */
	li reg_ZERO,0
	li reg_CODE,0
	li reg_CNAME,0
	li reg_LEXENV,0
	li reg_FDEFN,0
	li reg_OCFP,0
	li reg_LRA,0
	li reg_A0,0
	li reg_A1,0
	li reg_A2,0
	li reg_A3,0
	li reg_L0,0
	li reg_L1,0
	li reg_LIP,0
	lis reg_NULL,hi16(NIL)
	ori reg_NULL,reg_NULL,lo16(NIL)

	/* Turn on pseudo-atomic */

	li reg_ALLOC,4
	store(reg_ZERO,reg_NL4,_foreign_function_call_active)
	load(reg_NL4,_current_dynamic_space_free_pointer)
	add reg_ALLOC,reg_ALLOC,reg_NL4
	load(reg_BSP,_current_binding_stack_pointer)
	load(reg_CSP,_current_control_stack_pointer)
	load(reg_OCFP,_current_control_frame_pointer)

	/* No longer atomic, and check for interrupt */
	andi. reg_NL3, reg_ALLOC, 1
	subi reg_ALLOC,reg_ALLOC,4
	twnei reg_NL3, 0
	
	/* Pass in the arguments */

	mr reg_CFP,reg_NL1
	mr reg_LEXENV,reg_NL0
	lwz reg_A0,0(reg_CFP)
	lwz reg_A1,4(reg_CFP)
	lwz reg_A2,8(reg_CFP)
	lwz reg_A3,12(reg_CFP)

	/* Calculate LRA */
	lis reg_LRA,ha16(lra)
	addi reg_LRA,reg_LRA,lo16(lra)
	addi reg_LRA,reg_LRA,type_OtherPointer

	/* Function is an indirect closure */
	lwz reg_CODE,CLOSURE_FUNCTION_OFFSET(reg_LEXENV)
	/*
	 * The 6 is vm:function-code-offset, the 4 is
	 * the number of bytes in a lispobj.
	 */
	addi reg_LIP,reg_CODE,6*4-type_FunctionPointer
	mtctr reg_LIP
	slwi reg_NARGS,reg_NL2,2
	bctr
	.align 3
lra:
	.long type_ReturnPcHeader

	/* Blow off any extra values. */
	mr reg_CSP,reg_OCFP
	nop

	/* Return the one value. */

	mr REG(3),reg_A0

	/* Turn on  pseudo-atomic */
	li reg_NL3,-4
	la reg_ALLOC,4(reg_ALLOC)

	/* Store lisp state */
	clrrwi reg_NL1,reg_ALLOC,3
	store(reg_NL1,reg_NL2,_current_dynamic_space_free_pointer)
	/* store(reg_POLL,reg_NL2,poll_flag) */
	/* load(reg_NL2,current_thread) */
	store(reg_BSP,reg_NL2,_current_binding_stack_pointer)
	store(reg_CSP,reg_NL2,_current_control_stack_pointer)
	store(reg_CFP,reg_NL2,_current_control_frame_pointer)
	/* load(reg_POLL,saver2) */

	/* No longer in Lisp. */
	store(reg_NL1,reg_NL2,_foreign_function_call_active)

	/* Check for interrupt */
	andi. reg_NL3, reg_ALLOC, 1
	subi reg_ALLOC, reg_ALLOC, 4
	twnei reg_NL3,0
	
	/* Back to C */
	C_FULL_EPILOG
	blr
	SET_SIZE(call_into_lisp)


	GFUNCDEF(call_into_c)
	/* We're kind of low on unboxed, non-dedicated registers here:
	most of the unboxed registers may have outgoing C args in them.
	CFUNC is going to have to go in the CTR in a moment, anyway
	so we'll free it up soon.  reg_NFP is preserved by lisp if it
	has a meaningful value in it, so we can use it.  reg_NARGS is
	free when it's not holding a copy of the "real" reg_NL3, which
	gets tied up by the pseudo-atomic mechanism */
	mtctr reg_CFUNC
	mflr reg_LIP
	/* Build a lisp stack frame */
	mr reg_OCFP,reg_CFP
	mr reg_CFP,reg_CSP
	la reg_CSP,32(reg_CSP)
	stw reg_OCFP,0(reg_CFP)
	stw reg_CODE,8(reg_CFP)
	/* The pseudo-atomic mechanism wants to use reg_NL3, but that
	may be an outgoing C argument.  Copy reg_NL3 to something that's
	unboxed and -not- one of the C argument registers */
	mr reg_NARGS,reg_NL3
	
	/* Turn on pseudo-atomic */
	la reg_ALLOC,4(reg_ALLOC)

	/* Convert the return address to an offset and save it on the stack. */
	sub reg_NFP,reg_LIP,reg_CODE
	la reg_NFP,type_OtherPointer(reg_NFP)
	stw reg_NFP,4(reg_CFP)

	/* Store Lisp state */
	clrrwi reg_NFP,reg_ALLOC,3
	store(reg_NFP,reg_CFUNC,_current_dynamic_space_free_pointer)
	/* load(reg_CFUNC,current_thread) */
	
	store(reg_BSP,reg_CFUNC,_current_binding_stack_pointer)
	store(reg_CSP,reg_CFUNC,_current_control_stack_pointer)
	store(reg_CFP,reg_CFUNC,_current_control_frame_pointer)

	/* No longer in Lisp */
	store(reg_CSP,reg_CFUNC,_foreign_function_call_active)
	/* load(reg_POLL,saver2) */
	/* Disable pseudo-atomic; check pending interrupt */
	andi. reg_NL3, reg_ALLOC, 1
	subi reg_ALLOC, reg_ALLOC, 4
	twnei reg_NL3, 0
	
	mr reg_NL3,reg_NARGS

	/* PowerOpen (i.e. OS X) requires the callee address in r12
           (a.k.a. CFUNC), so move it back there, too. */
	mfctr reg_CFUNC
        /* Into C we go. */
	bctrl

	/* Re-establish NIL */
	lis reg_NULL,hi16(NIL)
	ori reg_NULL,reg_NULL,lo16(NIL)
	/* And reg_ZERO */
	li reg_ZERO,0

	/* If we GC'ed during the FF code (as the result of a callback ?)
	the tagged lisp registers may now contain garbage (since the
	registers were saved by C and not seen by the GC.)  Put something
	harmless in all such registers before allowing an interrupt */
	li reg_CODE,0
	li reg_CNAME,0
	li reg_LEXENV,0
	/* reg_OCFP was pointing to a control stack frame & was preserved by C */
	li reg_LRA,0
	li reg_A0,0
	li reg_A1,0
	li reg_A2,0
	li reg_A3,0
	li reg_L0,0
	li reg_L1,0
	li reg_LIP,0

	/* Atomic ... */
#if 0	
	li reg_NL3,-4
#endif	
	li reg_ALLOC,4

	/* No long in foreign function call. */
	store(reg_ZERO,reg_NL2,_foreign_function_call_active)

	/* The free pointer may have moved */
	load(reg_NL4,_current_dynamic_space_free_pointer)
	add reg_ALLOC,reg_ALLOC,reg_NL4

	/* The BSP wasn't preserved by C, so load it */
	load(reg_BSP,_current_binding_stack_pointer)

	/* Other lisp stack/frame pointers were preserved by C.
	I can't imagine why they'd have moved */

	/* Get the return address back. */
	lwz reg_LIP,4(reg_CFP)
	lwz reg_CODE,8(reg_CFP)
	add reg_LIP,reg_CODE,reg_LIP
	la reg_LIP,-type_OtherPointer(reg_LIP)

	/* No longer atomic */
	andi. reg_NL3, reg_ALLOC, 1
	subi reg_ALLOC, reg_ALLOC, 4
	twnei reg_NL3, 0
	
	mtlr reg_LIP
	
	/* Reset the lisp stack. */
	mr reg_CSP,reg_CFP
	mr reg_CFP,reg_OCFP
	
	/* And back into Lisp. */
	blr

	SET_SIZE(call_into_c)

	GFUNCDEF(xundefined_tramp)
	.globl _undefined_tramp
	.byte 0
_undefined_tramp:	
	.byte 0, 0, type_FunctionHeader
	.long _undefined_tramp	/* self slot */
	.long NIL		/* next slot */
	.long NIL		/* name slot */
	.long NIL		/* arglist slot */
	.long NIL		/* type slot */

	twllei reg_ZERO,trap_Cerror
        /* Number of argument bytes */
	.byte 4
	.byte UNDEFINED_SYMBOL_ERROR
        /* Escape to create 16bit number from following two bytes, 
	   in little-endian order */
	.byte 254
	.byte SC_OFFSET_LO(sc_DescriptorReg, reg_FDEFN_NUM)
	.byte SC_OFFSET_HI(sc_DescriptorReg, reg_FDEFN_NUM)
	/*
	.byte sc_DescriptorReg+0x40
	.byte 1
	*/
	.align 2
1:	lwz reg_CODE,FDEFN_RAW_ADDR_OFFSET(reg_FDEFN)
	la reg_LIP,FUNCTION_CODE_OFFSET(reg_CODE)
	mtctr reg_LIP
	bctr

	mr reg_CSP, reg_CFP
	b 1b
	
	SET_SIZE(xundefined_tramp)

	GFUNCDEF(xclosure_tramp)
	.globl _closure_tramp
	.byte 0
_closure_tramp:
	.byte 0,0,type_FunctionHeader	/* Header */
	.long _closure_tramp
	.long NIL 
	.long NIL
	.long NIL
	.long NIL

	lwz reg_LEXENV,FDEFN_FUNCTION_OFFSET(reg_FDEFN)
	lwz reg_CODE,CLOSURE_FUNCTION_OFFSET(reg_LEXENV)
	la reg_LIP,FUNCTION_CODE_OFFSET(reg_CODE)
	mtctr reg_LIP
	bctr

	SET_SIZE(xclosure_tramp)

/*
 * Function-end breakpoint magic.  See MAKE-BOGUS-LRA in
 * code/debug-int.lisp.
 */			
	.text
	.align 3
	.globl _function_end_breakpoint_guts
_function_end_breakpoint_guts:	
	.long	type_ReturnPcHeader
	b	1f
	/* This NOP is necessary for the return convention, I think */
	nop	
	/*
	 * Are these really necessary? I'm cargo-culting from the sparc port 
	 */
	mr	reg_OCFP, reg_CSP
	addi	reg_CSP, reg_CSP, 4
	li	reg_NARGS, 4
	mr	reg_A1, reg_NULL	
	mr	reg_A2, reg_NULL
	mr	reg_A3, reg_NULL
1:					
	.globl _function_end_breakpoint_trap
_function_end_breakpoint_trap:
	twllei	reg_ZERO, trap_FunctionEndBreakpoint
	b	1b
	.globl _function_end_breakpoint_end
_function_end_breakpoint_end:


	GFUNCDEF(ppc_flush_cache_line)
	dcbf 0,REG(3)
	sync
	icbi 0,REG(3)
	sync
	isync
	blr
	SET_SIZE(ppc_flush_cache_line)

	GFUNCDEF(do_pending_interrupt)
	twllei	reg_ZERO, trap_PendingInterrupt
	blr
	SET_SIZE(do_pending_interrupt)
	
#ifdef LINKAGE_TABLE
/*
 * Call into C code to resolve a linkage entry.
 *
 * We get here by Lisp calling call_into_c with an address of the
 * desired function which is contained in the register reg_CFUNC (aka
 * %i4, aka %r28).  This is the address of the entry in the linkage
 * table, which is what we need to figure out what function we really
 * wanted.
 *
 * Note that because we get here from call_into_c, all necessary live
 * registers have been saved, including FP registers.  Hence, no need
 * to save them.
 */
	.globl _lazy_resolve_linkage
	GFUNCDEF(resolve_linkage_tramp)
	/*
	 * We need to call lazy_resolve_linkage.  reg_A0 (= r24)
	 * (see ppc-arch.c) contains the address of the linkage entry.  
	 */
	/*
	 * Need to save all parameter regs to the stack because we 
	 * need them for later.  We save the FP registers too.  (Do
	 * we really need to?  lazy_resolve_linkage shouldn't call
	 * anything that uses FP registers.  It's safe to do so.)
	 */
	subi	r1, r1, 8*4+8+8*13
	stfd	f1, 0(r1)
	stfd	f2, 8(r1)
	stfd	f3, 16(r1)
	stfd	f4, 24(r1)
	stfd	f5, 32(r1)
	stfd	f6, 40(r1)
	stfd	f7, 48(r1)
	stfd	f8, 56(r1)
	stfd	f9, 64(r1)
	stfd	f10, 72(r1)
	stfd	f11, 80(r1)
	stfd	f12, 88(r1)
	stfd	f13, 96(r1)
	
	stw	r3, 104+0(r1)
	stw	r4, 104+4(r1)
	stw	r5, 104+8(r1)
	stw	r6, 104+12(r1)
	stw	r7, 104+16(r1)
	stw	r8, 104+20(r1)
	stw	r9, 104+24(r1)
	stw	r10, 104+28(r1)

	
	mr	reg_NL0, reg_A0

	stwu	r1, (-(24+4))(r1)
	/*
	 * At this point reg_NIL should be available to us.  
	 * Call lazy_resolve_linkage to figure out the real function address.
	 */
	lis	reg_NULL, hi16(_lazy_resolve_linkage)
	ori	reg_NULL, reg_NULL, lo16(_lazy_resolve_linkage)
	mtctr	reg_NULL
	mflr	reg_CODE
	bctrl

	addi	r1,r1,24+4
	
	/*
	 * The desired function is in r3 (NL0), so save it
	 * and restore the real arg parameters
	 */

	mtctr	reg_NL0
	lfd	f1, 0(r1)
	lfd	f2, 8(r1)
	lfd	f3, 16(r1)
	lfd	f4, 24(r1)
	lfd	f5, 32(r1)
	lfd	f6, 40(r1)
	lfd	f7, 48(r1)
	lfd	f8, 56(r1)
	lfd	f9, 64(r1)
	lfd	f10, 72(r1)
	lfd	f11, 80(r1)
	lfd	f12, 88(r1)
	lfd	f13, 96(r1)
	
	lwz	r3, 104+0(r1)
	lwz	r4, 104+4(r1)
	lwz	r5, 104+8(r1)
	lwz	r6, 104+12(r1)
	lwz	r7, 104+16(r1)
	lwz	r8, 104+20(r1)
	lwz	r9, 104+24(r1)
	lwz	r10, 104+28(r1)

	addi	r1,r1,8*4+8+8*13

	/*
	 * Got it, so we can now jump directly to the desired function.
	 * reg_NL0 contains the result.  Restore the stack and go!
	 */

	mtlr	reg_CODE
	
	/* Back to C */
	/*	mtlr	reg_CODE*/
	bctr
	
	SET_SIZE(resolve_linkage_tramp)
	

/*
 * When we get called, r3 (reg_NL0) contains the address of the
 * data_vector object which is a string naming the bad symbol.
 */
	GFUNCDEF(undefined_foreign_symbol_trap)

	/*
	   Need to restore all the global registers with the Lisp values that 
	   were saved away in call_into_c.  (This routine is only called from 
	   os_link_one_symbol, which is called from resolve_linkage_tramp, which 
	   is called from call_into_c.)

	   The global registers are volatile across function calls, so who
	   knows what values have been they contain now!
		
	*/

	load(reg_ALLOC, _current_dynamic_space_free_pointer)
        load(reg_BSP, _current_binding_stack_pointer)
        load(reg_CSP, _current_control_stack_pointer)
        load(reg_CFP, _current_control_frame_pointer)

	lis reg_NULL,hi16(NIL)
	ori reg_NULL,reg_NULL,lo16(NIL)
	/* And reg_ZERO */
	li reg_ZERO,0

	mr	reg_A0, reg_NL0
	twllei	reg_ZERO, trap_Error
	.byte	4	/* Number of argument bytes */
	.byte	UNDEFINED_FOREIGN_SYMBOL_ERROR
        /* Escape to create 16bit number from following two bytes, in 
	   little-endian order */
        .byte	254
	.byte	SC_OFFSET_LO(sc_DescriptorReg, reg_A0_NUM)
	.byte	SC_OFFSET_HI(sc_DescriptorReg, reg_A0_NUM)
	.align	4
					
#endif	

#ifdef GENCGC	
	.globl _fpu_save
	GFUNCDEF(fpu_save)
	stfd	f1, 0(r3)
	stfd	f2, 8(r3)
	stfd	f3, 16(r3)
	stfd	f4, 24(r3)
	stfd	f5, 32(r3)
	stfd	f6, 40(r3)
	stfd	f7, 48(r3)
	stfd	f8, 56(r3)
	stfd	f9, 64(r3)
	stfd	f10, 72(r3)
	stfd	f11, 80(r3)
	stfd	f12, 88(r3)
	stfd	f13, 96(r3)
	stfd	f14, 104(r3)
	stfd	f15, 112(r3)
	stfd	f16, 120(r3)
	stfd	f17, 128(r3)
	stfd	f18, 136(r3)
	stfd	f19, 144(r3)
	stfd	f20, 152(r3)
	stfd	f21, 160(r3)
	stfd	f22, 168(r3)
	stfd	f23, 176(r3)
	stfd	f24, 184(r3)
	stfd	f25, 192(r3)
	stfd	f26, 200(r3)
	stfd	f27, 208(r3)
	stfd	f28, 216(r3)
	stfd	f29, 224(r3)
	stfd	f30, 232(r3)
	stfd	f31, 240(r3)
	blr
	SET_SIZE(fpu_save)
	
	.globl	_fpu_restore
	GFUNCDEF(fpu_restore)
	lfd	f1, 0(r3)
	lfd	f2, 8(r3)
	lfd	f3, 16(r3)
	lfd	f4, 24(r3)
	lfd	f5, 32(r3)
	lfd	f6, 40(r3)
	lfd	f7, 48(r3)
	lfd	f8, 56(r3)
	lfd	f9, 64(r3)
	lfd	f10, 72(r3)
	lfd	f11, 80(r3)
	lfd	f12, 88(r3)
	lfd	f13, 96(r3)
	lfd	f14, 104(r3)
	lfd	f15, 112(r3)
	lfd	f16, 120(r3)
	lfd	f17, 128(r3)
	lfd	f18, 136(r3)
	lfd	f19, 144(r3)
	lfd	f20, 152(r3)
	lfd	f21, 160(r3)
	lfd	f22, 168(r3)
	lfd	f23, 176(r3)
	lfd	f24, 184(r3)
	lfd	f25, 192(r3)
	lfd	f26, 200(r3)
	lfd	f27, 208(r3)
	lfd	f28, 216(r3)
	lfd	f29, 224(r3)
	lfd	f30, 232(r3)
	lfd	f31, 240(r3)
	blr
	SET_SIZE(fpu_restore)
	
#endif
