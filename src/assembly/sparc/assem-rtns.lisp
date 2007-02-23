;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/assembly/sparc/assem-rtns.lisp,v 1.4 2005/02/11 21:02:32 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/assembly/sparc/assem-rtns.lisp,v 1.4 2005/02/11 21:02:32 rtoy Exp $
;;;
;;;
(in-package "SPARC")


;;;; Return-multiple with other than one value

#+assembler ;; we don't want a vop for this one.
(define-assembly-routine
    (return-multiple
     (:return-style :none))

     ;; These four are really arguments.
    ((:temp nvals any-reg nargs-offset)
     (:temp vals any-reg nl0-offset)
     (:temp ocfp any-reg nl1-offset)
     (:temp lra descriptor-reg lra-offset)

     ;; These are just needed to facilitate the transfer
     (:temp count any-reg nl2-offset)
     (:temp src any-reg nl3-offset)
     (:temp dst any-reg nl4-offset)
     (:temp temp descriptor-reg cname-offset)

     ;; These are needed so we can get at the register args.
     (:temp a0 descriptor-reg a0-offset)
     (:temp a1 descriptor-reg a1-offset)
     (:temp a2 descriptor-reg a2-offset)
     (:temp a3 descriptor-reg a3-offset)
     (:temp a4 descriptor-reg a4-offset)
     (:temp a5 descriptor-reg a5-offset))

  ;; Note, because of the way the return-multiple vop is written, we can
  ;; assume that we are never called with nvals == 1 and that a0 has already
  ;; been loaded.
  (inst cmp nvals)
  (inst b :le default-a0-and-on)
  (inst cmp nvals (fixnumize 2))
  (inst b :le default-a2-and-on)
  (inst ld a1 vals (* 1 vm:word-bytes))
  (inst cmp nvals (fixnumize 3))
  (inst b :le default-a3-and-on)
  (inst ld a2 vals (* 2 vm:word-bytes))
  (inst cmp nvals (fixnumize 4))
  (inst b :le default-a4-and-on)
  (inst ld a3 vals (* 3 vm:word-bytes))
  (inst cmp nvals (fixnumize 5))
  (inst b :le default-a5-and-on)
  (inst ld a4 vals (* 4 vm:word-bytes))
  (inst cmp nvals (fixnumize 6))
  (inst b :le done)
  (inst ld a5 vals (* 5 vm:word-bytes))

  ;; Copy the remaining args to the top of the stack.
  (inst add src vals (* 6 vm:word-bytes))
  (inst add dst cfp-tn (* 6 vm:word-bytes))
  (inst subcc count nvals (fixnumize 6))

  LOOP
  (inst ld temp src)
  (inst add src vm:word-bytes)
  (inst st temp dst)
  (inst add dst vm:word-bytes)
  (inst b :gt loop)
  (inst subcc count (fixnumize 1))
		
  (inst b done)
  (inst nop)

  DEFAULT-A0-AND-ON
  (inst move a0 null-tn)
  (inst move a1 null-tn)
  DEFAULT-A2-AND-ON
  (inst move a2 null-tn)
  DEFAULT-A3-AND-ON
  (inst move a3 null-tn)
  DEFAULT-A4-AND-ON
  (inst move a4 null-tn)
  DEFAULT-A5-AND-ON
  (inst move a5 null-tn)
  DONE
  
  ;; Clear the stack.
  (move ocfp-tn cfp-tn)
  (move cfp-tn ocfp)
  (inst add csp-tn ocfp-tn nvals)
  
  ;; Return.
  (lisp-return lra))



;;;; tail-call-variable.

#+assembler ;; no vop for this one either.
(define-assembly-routine
    (tail-call-variable
     (:return-style :none))

    ;; These are really args.
    ((:temp args any-reg nl0-offset)
     (:temp lexenv descriptor-reg lexenv-offset)

     ;; We need to compute this
     (:temp nargs any-reg nargs-offset)

     ;; These are needed by the blitting code.
     (:temp src any-reg nl1-offset)
     (:temp dst any-reg nl2-offset)
     (:temp count any-reg nl3-offset)
     (:temp temp descriptor-reg cname-offset)

     ;; These are needed so we can get at the register args.
     (:temp a0 descriptor-reg a0-offset)
     (:temp a1 descriptor-reg a1-offset)
     (:temp a2 descriptor-reg a2-offset)
     (:temp a3 descriptor-reg a3-offset)
     (:temp a4 descriptor-reg a4-offset)
     (:temp a5 descriptor-reg a5-offset))


  ;; Calculate NARGS (as a fixnum)
  (inst sub nargs csp-tn args)
     
  ;; Load the argument regs (must do this now, 'cause the blt might
  ;; trash these locations)
  (inst ld a0 args (* 0 vm:word-bytes))
  (inst ld a1 args (* 1 vm:word-bytes))
  (inst ld a2 args (* 2 vm:word-bytes))
  (inst ld a3 args (* 3 vm:word-bytes))
  (inst ld a4 args (* 4 vm:word-bytes))
  (inst ld a5 args (* 5 vm:word-bytes))

  ;; Calc SRC, DST, and COUNT
  (inst addcc count nargs (fixnumize (- register-arg-count)))
  (inst b :le done)
  (inst add src args (* vm:word-bytes register-arg-count))
  (inst add dst cfp-tn (* vm:word-bytes register-arg-count))
	
  LOOP
  ;; Copy one arg.
  (inst ld temp src)
  (inst add src src vm:word-bytes)
  (inst st temp dst)
  (inst addcc count (fixnumize -1))
  (inst b :gt loop)
  (inst add dst dst vm:word-bytes)
	
  DONE
  ;; We are done.  Do the jump.
  (loadw temp lexenv vm:closure-function-slot vm:function-pointer-type)
  (lisp-jump temp))



;;;; Non-local exit noise.

(define-assembly-routine (unwind
			  (:return-style :none)
			  (:translate %continue-unwind)
			  (:policy :fast-safe))
			 ((:arg block (any-reg descriptor-reg) a0-offset)
			  (:arg start (any-reg descriptor-reg) ocfp-offset)
			  (:arg count (any-reg descriptor-reg) nargs-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp cur-uwp any-reg nl0-offset)
			  (:temp next-uwp any-reg nl1-offset)
			  (:temp target-uwp any-reg nl2-offset))
  (declare (ignore start count))

  (let ((error (generate-error-code nil invalid-unwind-error)))
    (inst cmp block)
    (inst b :eq error))
  
  (load-symbol-value cur-uwp lisp::*current-unwind-protect-block*)
  (loadw target-uwp block vm:unwind-block-current-uwp-slot)
  (inst cmp cur-uwp target-uwp)
  (inst b :ne do-uwp)
  (inst nop)
      
  (move cur-uwp block)

  DO-EXIT
      
  (loadw cfp-tn cur-uwp vm:unwind-block-current-cont-slot)
  (loadw code-tn cur-uwp vm:unwind-block-current-code-slot)
  (loadw lra cur-uwp vm:unwind-block-entry-pc-slot)
  (lisp-return lra :frob-code nil)

  DO-UWP

  (loadw next-uwp cur-uwp vm:unwind-block-current-uwp-slot)
  (inst b do-exit)
  (store-symbol-value next-uwp lisp::*current-unwind-protect-block*))


(define-assembly-routine (throw
			  (:return-style :none))
			 ((:arg target descriptor-reg a0-offset)
			  (:arg start any-reg ocfp-offset)
			  (:arg count any-reg nargs-offset)
			  (:temp catch any-reg a1-offset)
			  (:temp tag descriptor-reg a2-offset)
			  (:temp temp non-descriptor-reg nl0-offset))
  
  (declare (ignore start count))

  (load-symbol-value catch lisp::*current-catch-block*)
  
  loop
  
  (let ((error (generate-error-code nil unseen-throw-tag-error target)))
    (inst cmp catch)
    (inst b :eq error)
    (inst nop))
  
  (loadw tag catch vm:catch-block-tag-slot)
  (inst cmp tag target)
  (inst b :eq exit)
  (inst nop)
  (loadw catch catch vm:catch-block-previous-catch-slot)
  (inst b loop)
  (inst nop)
  
  exit
  
  (move target catch)
  (inst li temp (make-fixup 'unwind :assembly-routine))
  (inst j temp)
  (inst nop))


