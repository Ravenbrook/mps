;;; -*- Package: RT; Log: c.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/assembly/rt/assem-rtns.lisp,v 1.7 2003/08/03 11:27:50 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/assembly/rt/assem-rtns.lisp,v 1.7 2003/08/03 11:27:50 gerd Exp $
;;;

(in-package "RT")



;;;; Non-local exit noise.

(define-assembly-routine (unwind (:translate %continue-unwind)
				 (:return-style :none)
				 (:policy :fast-safe))
			 ((:arg block (word-pointer-reg any-reg descriptor-reg)
				a0-offset)
			  (:arg start (word-pointer-reg any-reg descriptor-reg)
				ocfp-offset)
			  (:arg count (any-reg descriptor-reg) nargs-offset)
			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp cur-uwp any-reg nl0-offset)
			  (:temp next-uwp any-reg cname-offset)
			  (:temp target-uwp any-reg lexenv-offset))
  (declare (ignore start count))

  (let ((error (generate-error-code nil invalid-unwind-error)))
    (inst c block 0)
    (inst bc :eq error))

  (load-symbol-value cur-uwp lisp::*current-unwind-protect-block*)
  (loadw target-uwp block unwind-block-current-uwp-slot)
  (inst c cur-uwp target-uwp)
  (inst bnc :eq do-uwp)

  (move cur-uwp block)

  DO-EXIT

  (loadw cfp-tn cur-uwp unwind-block-current-cont-slot)
  (loadw code-tn cur-uwp unwind-block-current-code-slot)
  (loadw lra cur-uwp unwind-block-entry-pc-slot)
  (lisp-return lra lip :frob-code nil)

  DO-UWP

  (loadw next-uwp cur-uwp unwind-block-current-uwp-slot)
  (inst bx do-exit)
  (store-symbol-value next-uwp lisp::*current-unwind-protect-block*))

(define-assembly-routine (throw
			  (:return-style :none))
			 ((:arg target descriptor-reg a0-offset)
			  (:arg start word-pointer-reg ocfp-offset)
			  (:arg count any-reg nargs-offset)
			  (:temp catch any-reg a1-offset)
			  (:temp tag descriptor-reg a2-offset)
			  (:temp ndescr non-descriptor-reg nl0-offset))
  
  ;; These are needed by UNWIND, but not by us directly.
  (declare (ignore start count))

  (load-symbol-value catch lisp::*current-catch-block*)
  
  LOOP

  (let ((error (generate-error-code nil unseen-throw-tag-error target)))
    (inst c catch 0)
    (inst bc :eq error))

  (loadw tag catch catch-block-tag-slot)
  (inst c tag target)
  (inst bc :eq exit)
  (loadw catch catch catch-block-previous-catch-slot)
  (inst b loop)

  EXIT

  (move target catch)
  (inst cai ndescr (make-fixup 'unwind :assembly-routine))
  (inst b ndescr))



;;;; Return unknown multiple values (known not to be one value).

(define-assembly-routine really-return-multiple
			 ((:arg ocfp any-reg nl0-offset)
			  (:arg lra descriptor-reg lra-offset)
			  ;; Pointer into cstack, so looks like fixnum.
			  (:arg src any-reg cname-offset)
			  (:arg nvals any-reg nargs-offset)

			  (:temp lip interior-reg lip-offset)
			  (:temp count any-reg ocfp-offset)
			  ;; Pointer into cstack, so looks like fixnum.
			  (:temp dst any-reg lexenv-offset)
			  (:temp temp any-reg nfp-offset)
			  (:temp a0 descriptor-reg a0-offset)
			  (:temp a1 descriptor-reg a1-offset)
			  (:temp a2 descriptor-reg a2-offset))
  ;;
  ;; Load the register args, bailing out when we are done.
  (inst c nvals 0)
  (inst bc :eq default-a0-and-on)
  (loadw a0 src 0)
  (inst c nvals (fixnumize 1))
  (inst bc :eq default-a1-and-on)
  (loadw a1 src 1)
  (inst c nvals (fixnumize 2))
  (inst bc :eq default-a2-and-on)
  (loadw a2 src 2)
  ;;
  ;; Copy the remaining args to the top of the stack.
  (inst inc src (* word-bytes register-arg-count))
  (inst cal dst cfp-tn (* word-bytes register-arg-count))
  (inst s count nvals (fixnumize 3))
  (inst bnc :gt done)
  LOOP
  (loadw temp src)
  (inst inc src word-bytes)
  (storew temp dst)
  (inst inc dst word-bytes)
  (inst s count (fixnumize 1))
  (inst bc :gt loop)
  (inst b done)
  ;;
  ;; Default some number of registers.
  DEFAULT-A0-AND-ON
  (move a0 null-tn)
  DEFAULT-A1-AND-ON
  (move a1 null-tn)
  DEFAULT-A2-AND-ON
  (move a2 null-tn)
  ;;
  ;; Clear the stack.
  DONE
  (move count cfp-tn) ;Put pointer to the start of values in ocfp.
  (inst cas csp-tn nvals count) ;Adjust the stack top: <ptr-to-vals>+<nvals>.
  (move cfp-tn ocfp) ;Restore returnee's fp.
  ;;
  ;; Return.
  (lisp-return lra lip))



;;;; Copying more args on call (known to be at least one more arg.)

;;; REALLY-COPY-MORE-ARGS -- Assembler Routine.
;;;
;;; This miscop has weird register usage conventions, since it is called at the
;;; very beginning of entry to a more-arg XEP.  All of the registers used
;;; by the full call convention are in use, and must not be trashed, but none
;;; of the normal local & temporary registers are in use, since we haven't done
;;; anything yet.  The number of fixed arguments is passed in a3, since we
;;; can't trash a0..a2.
;;;
;;; The VOP COPY-MORE-ARG has allocated some temporaries for us: a3, l0, l1.

;;; The registers we can use are NL1, A3, L0 and L1.  We save L2 and L3 to give
;;; us some more registers to work with.  The caller saves the function return
;;; PC in L2 (the now unused NAME) so that it can be used for the miscop return
;;; PC.
;;;
;;; What we do is fairly simple: we copy the incoming more arguments (index >=
;;; a3) onto the top of the stack.  The loop runs backwards, since we are
;;; copying args upward on the stack, and the source and destination can
;;; overlap.
;;;
;;; This loop terminates when we either run out of more args to copy or we run
;;; out of stack arguments.  If we run out of stack arguments before we finish,
;;; then there are register more args.  In this case, we dispatch off of the
;;; number of fixed arguments (A3) to determine how many argument registers to
;;; store.
;;;
(define-assembly-routine really-copy-more-args
			 ;; Fixedargs contains the number of fixed arguments.
			 ((:arg fixedargs any-reg cname-offset)
			  (:temp count/src any-reg nfp-offset)
			  (:temp ocsp any-reg nl0-offset)
			  ;; Temporaries to get my hands on argument registers.
			  (:temp a0 any-reg a0-offset)
			  (:temp a1 any-reg a1-offset)
			  (:temp a2 any-reg a2-offset)
			  ;; We save the contents of these registers before
			  ;; using them.
			  (:temp srcstart any-reg lexenv-offset)
			  (:temp dstend any-reg lra-offset)
			  (:temp temp any-reg ocfp-offset))
  ;; First, save count/src (the NFP) on the number stack.  We need to do this
  ;; instead of with the other saves, because we need to use this register to
  ;; figure out where to save the others.  We can't same them all there,
  ;; because some of the others hold descriptor values.
  (storew count/src nsp-tn -1)
  ;; How many more args are there to copy to the stack?
  (move count/src nargs-tn)
  (inst s count/src fixedargs)
  ;; Save the current stack top as the start of the more args.
  (move ocsp csp-tn)
  ;; Bump the stack by the number of more args.
  (inst cas csp-tn csp-tn count/src)
  ;; Bump the stack three for the registers we have to save.
  (inst cal csp-tn csp-tn (* 3 word-bytes))

  ;; Dump the registers to make room for the temporaries we need.
  (storew srcstart csp-tn -3)
  (storew dstend csp-tn -2)
  (storew temp csp-tn -1)

  ;; Compute the source start, but remember we are copying from the end.
  ;; The start location is the last one we copy.
  (inst cal srcstart cfp-tn (* register-arg-count word-bytes))
  ;; Compute the source location of the last more arg.
  (inst cas count/src nargs-tn cfp-tn)
  (inst dec count/src word-bytes) ;not an exclusive end.
  ;; Compute destination end, which is the stack pointer minus three register
  ;; slots we saved and minus one to make it an inclusive end.
  (inst cal dstend csp-tn (* -4 word-bytes))

  ;; Copy some args.
  (inst b cma-test)
  CMA-LOOP
  (loadw temp count/src)
  (storew temp dstend)
  (inst dec count/src word-bytes)
  (inst dec dstend word-bytes)
  CMA-TEST
  ;; Are we totally done?
  (inst c dstend ocsp)
  (inst bc :lt CMA-DONE)
  ;; Are we done copying stack more args?
  (inst c count/src srcstart)
  (inst bnc :lt CMA-LOOP)
  
  ;; Figure out which argument registers may be more args.
  (inst c fixedargs (fixnumize 2))
  (inst bc :eq two-fixed-args)
  (inst c fixedargs (fixnumize 1))
  (inst bc :eq one-fixed-arg)
  
  ;; Copy register more args being careful to stop before running out of args.
  (storew a0 ocsp)
  (inst c nargs-tn (fixnumize 1))
  (inst bc :eq CMA-DONE)
  (inst inc ocsp word-bytes)
  ONE-FIXED-ARG
  (storew a1 ocsp)
  (inst c nargs-tn (fixnumize 2))
  (inst bc :eq CMA-DONE)
  (inst inc ocsp word-bytes)
  TWO-FIXED-ARGS
  (storew a2 ocsp)

  ;; Restore registers we dumped.
  CMA-DONE
  (loadw srcstart csp-tn -3)
  (loadw dstend csp-tn -2)
  (loadw temp csp-tn -1)
  (inst dec csp-tn (* 3 word-bytes))
  (loadw count/src nsp-tn -1)
  (inst b lip-tn))



;;;; Tail call variable.

(define-assembly-routine (really-tail-call-variable (:return-style :none))
			 ;; These are really args.
			 ((:temp args any-reg nl0-offset)
			  ;; These are needed by the blitting code.
			  (:temp count any-reg lra-offset)
			  (:temp dst any-reg lexenv-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp temp descriptor-reg cname-offset)
			  ;; These are needed so we can get at the register args.
			  (:temp a0 descriptor-reg a0-offset)
			  (:temp a1 descriptor-reg a1-offset)
			  (:temp a2 descriptor-reg a2-offset))
  ;; Calculate NARGS (as a fixnum).
  (move nargs csp-tn)
  (inst s nargs args)
  ;; Load the argument registers now because the argument moving might trash
  ;; these locations.
  (loadw a0 args 0)
  (loadw a1 args 1)
  (loadw a2 args 2)
  ;; Are we done?
  (inst c nargs (fixnumize register-arg-count))
  (inst bnc :gt done)
  ;; Calc DST and COUNT after saving those registers.
  (inst inc csp-tn (* word-bytes 2))
  (storew count csp-tn -1)
  (storew dst csp-tn -2)
  (inst a count nargs (fixnumize (- register-arg-count)))
  (inst a args (* vm:word-bytes register-arg-count))
  (inst a dst cfp-tn (* vm:word-bytes register-arg-count))
  LOOP
  ;; Copy one arg.
  (loadw temp args)
  (inst inc args vm:word-bytes)
  (storew temp dst)
  (inst s count (fixnumize 1))
  (inst bcx :gt loop)
  (inst inc dst vm:word-bytes)
  ;; Restore
  (loadw count csp-tn -1)
  (loadw dst csp-tn -2)
  (inst dec csp-tn (* word-bytes 2))
  DONE
  ;; We are done.  Do the jump.
  (loadw temp dst vm:closure-function-slot vm:function-pointer-type)
  (lisp-jump temp lip-tn))
