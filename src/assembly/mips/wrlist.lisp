;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/assembly/mips/wrlist.lisp,v 1.7 1994/10/31 04:56:40 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the assembly routines for write-list support.
;;;

(in-package "MIPS")

(define-assembly-routine
    (write-list-record)
    ((:arg object descriptor-reg a0-offset)
     (:arg offset non-descriptor-reg nl2-offset)
     (:arg value descriptor-reg a1-offset)

     (:temp nl0 non-descriptor-reg nl0-offset)
     (:temp nl1 non-descriptor-reg nl1-offset)

     (:temp nl3 non-descriptor-reg nl3-offset)
     (:temp nl4 non-descriptor-reg nl4-offset)
     (:temp nl5 non-descriptor-reg nl5-offset)
     (:temp nargs non-descriptor-reg nargs-offset)

     (:temp a2 any-reg a2-offset)
     (:temp a3 any-reg a3-offset)
     (:temp a4 any-reg a4-offset)
     (:temp a5 any-reg a5-offset)
     (:temp cname any-reg cname-offset)
     (:temp lexenv any-reg lexenv-offset)
     (:temp l0 any-reg l0-offset)
     (:temp l1 any-reg l1-offset)
     (:temp l2 any-reg l2-offset)
     (:temp l3 any-reg l3-offset)
     (:temp nfp any-reg nfp-offset)
     (:temp ocfp any-reg ocfp-offset)
     (:temp lra any-reg lra-offset))

  ;; The write into memory and the write into the write-list have to be atomic.
  (start-pseudo-atomic)

  TRY-AGAIN

  ;; First, make sure there is space available in the write list.
  (inst lw nl0 mutator-tn (* mutator-write-list-fill-pointer-slot word-bytes))
  (inst lw nl1 mutator-tn (* mutator-write-list-end-slot word-bytes))
  (inst nop)
  (inst bne nl0 nl1 space-available)
  (inst nop)

  ;; No space, so we have to call out to gengc_FlushWriteList.

  ;; We have to save all the descriptor registers on the stack in case
  ;; we end up collecting garbage while in C.

  ;; Save all lisp regs on the stack, so the garbage collector can find them.
  (save-regs-on-stack (object value a2 a3 a4 a5 cname lexenv
		       l0 l1 l2 l3 nfp ocfp code-tn lra null-tn)

    ;; Convert the return address into an offset.  We don't have to save l0
    ;; across the call-out because it is one of the saved regs.
    (inst subu l0 lip-tn code-tn)

    ;; Decache all the lisp state from the mutator structure.
    (storew csp-tn mutator-tn mutator-control-stack-pointer-slot)
    (storew cfp-tn mutator-tn mutator-control-frame-pointer-slot)

    ;; Indicate that we are now in foreign-function-call land.
    (storew csp-tn mutator-tn mutator-foreign-fn-call-active-slot)

    ;; Turn off pseudo-atomic.
    (end-pseudo-atomic nl0)

    ;; Move offset to one of the saved registers.
    (inst move l1 offset)

    ;; Move the other non-descriptor regs into saved regs.
    (inst move l2 nl3)
    (inst move l3 nl4)
    (inst move nfp nl5)
    (inst move ocfp nargs)

    ;; Allocate the stack frame.
    (inst subu nsp-tn nsp-tn (* 4 word-bytes))

    ;; Restore GP
    (inst li null-tn (make-fixup "_gp" :foreign))

    ;; Call-out to gengc_Allocate, setting up the single argument.
    (inst jal (make-fixup "gengc_FlushWriteList" :foreign))
    (inst move nl0 mutator-tn)

    ;; Deallocate the stack frame.
    (inst addu nsp-tn nsp-tn (* 4 word-bytes))

    ;; Restore offset from the saved reg.
    (inst move offset l1)

    ;; Restore the other non-descriptor regs.
    (inst move nl3 l2)
    (inst move nl4 l3)
    (inst move nl5 nfp)
    (inst move nargs ocfp)

    ;; Turn pseudo-atomic back on.
    (start-pseudo-atomic)

    ;; No longer in foreign-foreign-call land.
    (storew zero-tn mutator-tn mutator-foreign-fn-call-active-slot)

    ;; Cache lisp state from the mutator structure.
    (loadw csp-tn mutator-tn mutator-control-stack-pointer-slot)
    (loadw cfp-tn mutator-tn mutator-control-frame-pointer-slot)

    ;; We can't restore the return address yet, because code hasn't been
    ;; restored.  But if we wait until after code has been restored,
    ;; then l0 will have been trashed.  So we have to move the value
    ;; out of l0 and into nl0.
    (inst move nl0 l0)

    ;; Restore all the saved regs. (by dropping out of the save-regs-on-stack)
    )

  ;; Regenerate the return address from the saved offset.
  (inst addu lip-tn nl0 code-tn)
    
  ;; Because we turned off pseudo-atomic, we might have taken an interrupt,
  ;; so we can't be sure the write-list will be empty.
  (inst b try-again)
  (inst nop)

  SPACE-AVAILABLE

  (inst addu nl1 object offset)
  (inst sw nl1 nl0)
  (inst sw value nl1)
  (inst addu nl0 4)
  (inst sw nl0 mutator-tn (* mutator-write-list-fill-pointer-slot word-bytes))

  (end-pseudo-atomic nl0))


(define-assembly-routine
    (set-fdefn-function)
    ((:arg fdefn descriptor-reg a0-offset)
     (:arg function descriptor-reg a1-offset)

     (:temp nl0 non-descriptor-reg nl0-offset)
     (:temp nl1 non-descriptor-reg nl1-offset)

     (:temp nl2 non-descriptor-reg nl2-offset)
     (:temp nl3 non-descriptor-reg nl3-offset)
     (:temp nl4 non-descriptor-reg nl4-offset)
     (:temp nl5 non-descriptor-reg nl5-offset)
     (:temp nargs non-descriptor-reg nargs-offset)

     (:temp a2 any-reg a2-offset)
     (:temp a3 any-reg a3-offset)
     (:temp a4 any-reg a4-offset)
     (:temp a5 any-reg a5-offset)
     (:temp cname any-reg cname-offset)
     (:temp lexenv any-reg lexenv-offset)
     (:temp l0 any-reg l0-offset)
     (:temp l1 any-reg l1-offset)
     (:temp l2 any-reg l2-offset)
     (:temp l3 any-reg l3-offset)
     (:temp nfp any-reg nfp-offset)
     (:temp ocfp any-reg ocfp-offset)
     (:temp lra any-reg lra-offset))

  ;; The write into memory and the write into the write-list have to be atomic.
  (start-pseudo-atomic)

  ;; First, check to see if we don't need to make a write-list entry because
  ;; the fdefn object is ephemeral.
  (inst srl nl0 fdefn 30)
  (inst bne nl0 zero-tn just-do-write)
  (inst nop)

  TRY-AGAIN

  ;; Now make sure there is space available in the write list.
  (inst lw nl0 mutator-tn (* mutator-write-list-fill-pointer-slot word-bytes))
  (inst lw nl1 mutator-tn (* mutator-write-list-end-slot word-bytes))
  (inst nop)
  (inst bne nl0 nl1 space-available)
  (inst nop)

  ;; No space, so we have to call out to gengc_FlushWriteList.

  ;; We have to save all the descriptor registers on the stack in case
  ;; we end up collecting garbage while in C.

  ;; Save all lisp regs on the stack, so the garbage collector can find them.
  (save-regs-on-stack (fdefn function a2 a3 a4 a5 cname lexenv
		       l0 l1 l2 l3 nfp ocfp code-tn lra null-tn)

    ;; Convert the return address into an offset.  We don't have to save l0
    ;; across the call-out because it is one of the saved regs.
    (inst subu l0 lip-tn code-tn)

    ;; Decache all the lisp state from the mutator structure.
    (storew csp-tn mutator-tn mutator-control-stack-pointer-slot)
    (storew cfp-tn mutator-tn mutator-control-frame-pointer-slot)

    ;; Indicate that we are now in foreign-function-call land.
    (storew csp-tn mutator-tn mutator-foreign-fn-call-active-slot)

    ;; Turn off pseudo-atomic.
    (end-pseudo-atomic nl0)

    ;; Move the non-descriptor regs into saved regs.
    (inst move l1 nl2)
    (inst move l2 nl3)
    (inst move l3 nl4)
    (inst move nfp nl5)
    (inst move ocfp nargs)

    ;; Allocate the stack frame.
    (inst subu nsp-tn nsp-tn (* 4 word-bytes))

    ;; Restore GP
    (inst li null-tn (make-fixup "_gp" :foreign))

    ;; Call-out to gengc_Allocate, setting up the single argument.
    (inst jal (make-fixup "gengc_FlushWriteList" :foreign))
    (inst move nl0 mutator-tn)

    ;; Deallocate the stack frame.
    (inst addu nsp-tn nsp-tn (* 4 word-bytes))

    ;; Restore the non-descriptor regs.
    (inst move nl2 l1)
    (inst move nl3 l2)
    (inst move nl4 l3)
    (inst move nl5 nfp)
    (inst move nargs ocfp)

    ;; Turn pseudo-atomic back on.
    (start-pseudo-atomic)

    ;; No longer in foreign-foreign-call land.
    (storew zero-tn mutator-tn mutator-foreign-fn-call-active-slot)

    ;; Cache lisp state from the mutator structure.
    (loadw csp-tn mutator-tn mutator-control-stack-pointer-slot)
    (loadw cfp-tn mutator-tn mutator-control-frame-pointer-slot)

    ;; We can't restore the return address yet, because code hasn't been
    ;; restored.  But if we wait until after code has been restored,
    ;; then l0 will have been trashed.  So we have to move the value
    ;; out of l0 and into nl0.
    (inst move nl0 l0)

    ;; Restore all the saved regs. (by dropping out of the save-regs-on-stack)
    )

  ;; Regenerate the return address from the saved offset.
  (inst addu lip-tn nl0 code-tn)
    
  ;; Because we turned off pseudo-atomic, we might have taken an interrupt,
  ;; so we can't be sure the write-list will be empty.
  (inst b try-again)
  (inst nop)

  SPACE-AVAILABLE

  (inst subu nl1 fdefn other-pointer-type)
  (inst sw nl1 nl0)
  (inst addu nl0 4)
  (inst sw nl0 mutator-tn (* mutator-write-list-fill-pointer-slot word-bytes))

  JUST-DO-WRITE

  (load-type nl0 function (- vm:function-pointer-type))
  (inst nop)
  (inst xor nl0 vm:closure-header-type)
  (inst beq nl0 zero-tn closure)
  (inst xor nl0 (logxor closure-header-type funcallable-instance-header-type))
  (inst beq nl0 zero-tn closure)
  (inst xor nl0 (logxor funcallable-instance-header-type function-header-type))
  (inst beq nl0 zero-tn normal-fn)
  (inst addu nl1 function
	(- (ash vm:function-header-code-offset vm:word-shift)
	   vm:function-pointer-type))
  (end-pseudo-atomic nl0)
  (error-call nil kernel:object-not-function-error function)

  CLOSURE
  (inst li nl1 (make-fixup "closure_tramp" :foreign))

  NORMAL-FN
  (storew function fdefn fdefn-function-slot other-pointer-type)
  (storew nl1 fdefn fdefn-raw-addr-slot other-pointer-type)

  (end-pseudo-atomic nl0))
