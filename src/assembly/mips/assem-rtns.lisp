;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/assembly/mips/assem-rtns.lisp,v 1.33 2003/08/03 11:27:51 gerd Exp $")
;;;
;;; **********************************************************************
;;;
(in-package "MIPS")


;;;; Return-multiple with other than one value

#+assembler ;; we don't want a vop for this one.
(define-assembly-routine
    (return-multiple
     (:return-style :none))

     ;; These four are really arguments.
    ((:temp nvals any-reg nargs-offset)
     (:temp vals any-reg nl0-offset)
     (:temp ocfp any-reg nl1-offset)
     #-gengc (:temp lra descriptor-reg lra-offset)
     #+gengc (:temp ra descriptor-reg ra-offset)

     ;; These are just needed to facilitate the transfer
     #-gengc (:temp lip interior-reg lip-offset)
     (:temp count any-reg nl2-offset)
     (:temp dst any-reg nl4-offset)
     (:temp temp descriptor-reg l0-offset)

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
  (inst blez nvals default-a0-and-on)
  (inst subu count nvals (fixnumize 2))
  (inst blez count default-a2-and-on)
  (inst lw a1 vals (* 1 vm:word-bytes))
  (inst subu count (fixnumize 1))
  (inst blez count default-a3-and-on)
  (inst lw a2 vals (* 2 vm:word-bytes))
  (inst subu count (fixnumize 1))
  (inst blez count default-a4-and-on)
  (inst lw a3 vals (* 3 vm:word-bytes))
  (inst subu count (fixnumize 1))
  (inst blez count default-a5-and-on)
  (inst lw a4 vals (* 4 vm:word-bytes))
  (inst subu count (fixnumize 1))
  (inst blez count done)
  (inst lw a5 vals (* 5 vm:word-bytes))

  ;; Copy the remaining args to the top of the stack.
  (inst addu vals vals (* 6 vm:word-bytes))
  (inst addu dst cfp-tn (* 6 vm:word-bytes))

  LOOP
  (inst lw temp vals)
  (inst addu vals vm:word-bytes)
  (inst sw temp dst)
  (inst subu count (fixnumize 1))
  (inst bne count zero-tn loop)
  (inst addu dst vm:word-bytes)
		
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
  (inst addu csp-tn ocfp-tn nvals)
  
  ;; Return.
  #-gengc
  (lisp-return lra lip)
  #+gengc
  (progn
    (inst j ra)
    (inst nop)))


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
     (:temp count any-reg cfunc-offset)
     (:temp temp descriptor-reg l0-offset)

     ;; Needed for the jump
     (:temp lip interior-reg lip-offset)

     ;; These are needed so we can get at the register args.
     (:temp a0 descriptor-reg a0-offset)
     (:temp a1 descriptor-reg a1-offset)
     (:temp a2 descriptor-reg a2-offset)
     (:temp a3 descriptor-reg a3-offset)
     (:temp a4 descriptor-reg a4-offset)
     (:temp a5 descriptor-reg a5-offset))


  ;; Calculate NARGS (as a fixnum)
  (inst subu nargs csp-tn args)
     
  ;; Load the argument regs (must do this now, 'cause the blt might
  ;; trash these locations)
  (inst lw a0 args (* 0 vm:word-bytes))
  (inst lw a1 args (* 1 vm:word-bytes))
  (inst lw a2 args (* 2 vm:word-bytes))
  (inst lw a3 args (* 3 vm:word-bytes))
  (inst lw a4 args (* 4 vm:word-bytes))
  (inst lw a5 args (* 5 vm:word-bytes))

  ;; Calc SRC, DST, and COUNT
  (inst addu count nargs (fixnumize (- register-arg-count)))
  (inst blez count done)
  (inst addu src args (* vm:word-bytes register-arg-count))
  (inst addu dst cfp-tn (* vm:word-bytes register-arg-count))
	
  LOOP
  ;; Copy one arg.
  (inst lw temp src)
  (inst addu src src vm:word-bytes)
  (inst sw temp dst)
  (inst addu count (fixnumize -1))
  (inst bgtz count loop)
  (inst addu dst dst vm:word-bytes)
	
  DONE
  ;; We are done.  Do the jump.
  #-gengc
  (progn
    (loadw temp lexenv vm:closure-function-slot vm:function-pointer-type)
    (lisp-jump temp lip))
  #+gengc
  (progn
    (loadw lip lexenv function-entry-point-slot function-pointer-type)
    (inst j lip)
    (inst nop)))


;;;; Non-local exit noise.

(define-assembly-routine
    (unwind
     (:translate %continue-unwind)
     (:policy :fast-safe))
    ((:arg block (any-reg descriptor-reg) a0-offset)
     (:arg start (any-reg descriptor-reg) ocfp-offset)
     (:arg count (any-reg descriptor-reg) nargs-offset)
     #-gengc (:temp lip interior-reg lip-offset)
     #-gengc (:temp lra descriptor-reg lra-offset)
     #+gengc (:temp temp any-reg l0-offset)
     (:temp cur-uwp any-reg nl0-offset)
     (:temp next-uwp any-reg nl1-offset)
     (:temp target-uwp any-reg nl2-offset))
  (declare (ignore start count))

  (let ((error (generate-error-code nil invalid-unwind-error)))
    (inst beq block zero-tn error))
  
  #-gengc (load-symbol-value cur-uwp lisp::*current-unwind-protect-block*)
  #+gengc (loadw cur-uwp mutator-tn mutator-current-unwind-protect-slot)
  (loadw target-uwp block vm:unwind-block-current-uwp-slot)
  (inst bne cur-uwp target-uwp do-uwp)
  (inst nop)
      
  (move cur-uwp block)

  do-exit
      
  (loadw cfp-tn cur-uwp vm:unwind-block-current-cont-slot)
  (loadw code-tn cur-uwp vm:unwind-block-current-code-slot)
  #-gengc
  (progn
    (loadw lra cur-uwp vm:unwind-block-entry-pc-slot)
    (lisp-return lra lip :frob-code nil))
  #+gengc
  (progn
    (loadw temp cur-uwp vm:unwind-block-entry-pc-slot)
    (inst j temp)
    (inst nop))

  do-uwp

  (loadw next-uwp cur-uwp vm:unwind-block-current-uwp-slot)
  (inst b do-exit)
  #-gengc (store-symbol-value next-uwp lisp::*current-unwind-protect-block*)
  #+gengc (storew next-uwp mutator-tn mutator-current-unwind-protect-slot))


(define-assembly-routine
    throw
    ((:arg target descriptor-reg a0-offset)
     (:arg start any-reg ocfp-offset)
     (:arg count any-reg nargs-offset)
     (:temp catch any-reg a1-offset)
     (:temp tag descriptor-reg a2-offset))
  
  (progn start count) ; We just need them in the registers.

  #-gengc (load-symbol-value catch lisp::*current-catch-block*)
  #+gengc (loadw catch mutator-tn mutator-current-catch-block-slot)
  
  loop
  
  (let ((error (generate-error-code nil unseen-throw-tag-error target)))
    (inst beq catch zero-tn error)
    (inst nop))
  
  (loadw tag catch vm:catch-block-tag-slot)
  (inst beq tag target exit)
  (inst nop)
  (loadw catch catch vm:catch-block-previous-catch-slot)
  (inst b loop)
  (inst nop)
  
  exit
  
  (move target catch)
  (inst j (make-fixup 'unwind :assembly-routine))
  (inst nop))
