;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/mips/nlx.lisp,v 1.22 2003/08/03 11:27:48 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the definitions of VOPs used for non-local exit
;;; (throw, lexical exit, etc.)
;;;
;;; Written by Rob MacLachlan
;;;
(in-package "MIPS")

;;; MAKE-NLX-SP-TN  --  Interface
;;;
;;;    Make an environment-live stack TN for saving the SP for NLX entry.
;;;
(def-vm-support-routine make-nlx-sp-tn (env)
  (environment-live-tn
   (make-representation-tn *fixnum-primitive-type* immediate-arg-scn)
   env))

;;; Make-NLX-Entry-Argument-Start-Location  --  Interface
;;;
;;;    Make a TN for the argument count passing location for a
;;; non-local entry.
;;;
(def-vm-support-routine make-nlx-entry-argument-start-location ()
  (make-wired-tn *fixnum-primitive-type* immediate-arg-scn ocfp-offset))


;;; Save and restore dynamic environment.
;;;
;;;    These VOPs are used in the reentered function to restore the appropriate
;;; dynamic environment.  Currently we only save the Current-Catch and binding
;;; stack pointer.  We don't need to save/restore the current unwind-protect,
;;; since unwind-protects are implicitly processed during unwinding.  If there
;;; were any additional stacks, then this would be the place to restore the top
;;; pointers.


;;; Make-Dynamic-State-TNs  --  Interface
;;;
;;;    Return a list of TNs that can be used to snapshot the dynamic state for
;;; use with the Save/Restore-Dynamic-Environment VOPs.
;;;
(def-vm-support-routine make-dynamic-state-tns ()
  (make-n-tns 4 *any-primitive-type*))

(define-vop (save-dynamic-state)
  (:results (catch :scs (descriptor-reg))
	    (nfp :scs (descriptor-reg))
	    (nsp :scs (descriptor-reg))
	    (eval :scs (descriptor-reg)))
  (:vop-var vop)
  (:generator 13
    #-gengc
    (load-symbol-value catch lisp::*current-catch-block*)
    #+gengc
    (loadw catch mutator-tn mutator-current-catch-block-slot)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(move nfp cur-nfp)))
    (move nsp nsp-tn)
    #-gengc
    (load-symbol-value eval lisp::*eval-stack-top*)
    #+gengc
    (loadw eval mutator-tn mutator-eval-stack-top-slot)))

(define-vop (restore-dynamic-state)
  (:args (catch :scs (descriptor-reg))
	 (nfp :scs (descriptor-reg))
	 (nsp :scs (descriptor-reg))
	 (eval :scs (descriptor-reg)))
  (:vop-var vop)
  (:generator 10
    #-gengc
    (store-symbol-value catch lisp::*current-catch-block*)
    #+gengc
    (storew catch mutator-tn mutator-current-catch-block-slot)
    #-gengc
    (store-symbol-value eval lisp::*eval-stack-top*)
    #+gengc
    (storew eval mutator-tn mutator-eval-stack-top-slot)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(move cur-nfp nfp)))
    (move nsp-tn nsp)))

(define-vop (current-stack-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (move res csp-tn)))

(define-vop (current-binding-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (move res bsp-tn)))



;;;; Unwind block hackery:

;;; Compute the address of the catch block from its TN, then store into the
;;; block the current Fp, Env, Unwind-Protect, and the entry PC.
;;;
(define-vop (make-unwind-block)
  (:args (tn))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 22
    (inst addu block cfp-tn (* (tn-offset tn) vm:word-bytes))
    #-gengc
    (load-symbol-value temp lisp::*current-unwind-protect-block*)
    #+gengc
    (loadw temp mutator-tn mutator-current-unwind-protect-slot)
    (storew temp block vm:unwind-block-current-uwp-slot)
    (storew cfp-tn block vm:unwind-block-current-cont-slot)
    (storew code-tn block vm:unwind-block-current-code-slot)
    #-gengc
    (inst compute-lra-from-code temp code-tn entry-label ndescr)
    #+gengc
    (inst compute-ra-from-code temp code-tn entry-label ndescr)
    (storew temp block vm:catch-block-entry-pc-slot)))


;;; Like Make-Unwind-Block, except that we also store in the specified tag, and
;;; link the block into the Current-Catch list.
;;;
(define-vop (make-catch-block)
  (:args (tn)
	 (tag :scs (descriptor-reg)))
  (:info entry-label)
  (:results (block :scs (any-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :target block :to (:result 0)) result)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 44
    (inst addu result cfp-tn (* (tn-offset tn) vm:word-bytes))
    #-gengc
    (load-symbol-value temp lisp::*current-unwind-protect-block*)
    #+gengc
    (loadw temp mutator-tn mutator-current-unwind-protect-slot)
    (storew temp result vm:catch-block-current-uwp-slot)
    (storew cfp-tn result vm:catch-block-current-cont-slot)
    (storew code-tn result vm:catch-block-current-code-slot)
    #-gengc
    (inst compute-lra-from-code temp code-tn entry-label ndescr)
    #+gengc
    (inst compute-ra-from-code temp code-tn entry-label ndescr)
    (storew temp result vm:catch-block-entry-pc-slot)

    (storew tag result vm:catch-block-tag-slot)
    #-gengc
    (load-symbol-value temp lisp::*current-catch-block*)
    #+gengc
    (loadw temp mutator-tn mutator-current-catch-block-slot)
    (storew temp result vm:catch-block-previous-catch-slot)
    #-gengc
    (store-symbol-value result lisp::*current-catch-block*)
    #+gengc
    (storew result mutator-tn mutator-current-catch-block-slot)

    (move block result)))


;;; Just set the current unwind-protect to TN's address.  This instantiates an
;;; unwind block as an unwind-protect.
;;;
(define-vop (set-unwind-protect)
  (:args (tn))
  (:temporary (:scs (descriptor-reg)) new-uwp)
  (:generator 7
    (inst addu new-uwp cfp-tn (* (tn-offset tn) vm:word-bytes))
    #-gengc
    (store-symbol-value new-uwp lisp::*current-unwind-protect-block*)
    #+gengc
    (storew new-uwp mutator-tn mutator-current-unwind-protect-slot)))


(define-vop (unlink-catch-block)
  (:temporary (:scs (any-reg)) block)
  (:policy :fast-safe)
  (:translate %catch-breakup)
  (:generator 17
    #-gengc
    (load-symbol-value block lisp::*current-catch-block*)
    #+gengc
    (loadw block mutator-tn mutator-current-catch-block-slot)
    (loadw block block vm:catch-block-previous-catch-slot)
    #-gengc
    (store-symbol-value block lisp::*current-catch-block*)
    #+gengc
    (storew block mutator-tn mutator-current-catch-block-slot)))

(define-vop (unlink-unwind-protect)
  (:temporary (:scs (any-reg)) block)
  (:policy :fast-safe)
  (:translate %unwind-protect-breakup)
  (:generator 17
    #-gengc
    (load-symbol-value block lisp::*current-unwind-protect-block*)
    #+gengc
    (loadw block mutator-tn mutator-current-unwind-protect-slot)
    (loadw block block vm:unwind-block-current-uwp-slot)
    #-gengc
    (store-symbol-value block lisp::*current-unwind-protect-block*)
    #+gengc
    (storew block mutator-tn mutator-current-unwind-protect-slot)))


;;;; NLX entry VOPs:


(define-vop (nlx-entry)
  (:args (sp) ; Note: we can't list an sc-restriction, 'cause any load vops
	      ; would be inserted before the LRA.
	 (start)
	 (count))
  (:results (values :more t))
  (:temporary (:scs (descriptor-reg)) move-temp)
  (:info label nvals)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-return-pc label)
    (note-this-location vop :non-local-entry)
    (cond ((zerop nvals))
	  ((= nvals 1)
	   (let ((no-values (gen-label)))
	     (inst beq count zero-tn no-values)
	     (move (tn-ref-tn values) null-tn)
	     (loadw (tn-ref-tn values) start)
	     (emit-label no-values)))
	  (t
	   (collect ((defaults))
	     (do ((i 0 (1+ i))
		  (tn-ref values (tn-ref-across tn-ref)))
		 ((null tn-ref))
	       (let ((default-lab (gen-label))
		     (tn (tn-ref-tn tn-ref)))
		 (defaults (cons default-lab tn))
		 
		 (inst beq count zero-tn default-lab)
		 (inst addu count count (fixnumize -1))
		 (sc-case tn
			  ((descriptor-reg any-reg)
			   (loadw tn start i))
			  (control-stack
			   (loadw move-temp start i)
			   (store-stack-tn tn move-temp)))))
	     
	     (let ((defaulting-done (gen-label)))
	       
	       (emit-label defaulting-done)
	       
	       (assemble (*elsewhere*)
		 (dolist (def (defaults))
		   (emit-label (car def))
		   (let ((tn (cdr def)))
		     (sc-case tn
			      ((descriptor-reg any-reg)
			       (move tn null-tn))
			      (control-stack
			       (store-stack-tn tn null-tn)))))
		 (inst b defaulting-done)
		 (inst nop))))))
    (load-stack-tn csp-tn sp)))


(define-vop (nlx-entry-multiple)
  (:args (top :target dst) (start :target src) (count :target num))
  ;; Again, no SC restrictions for the args, 'cause the loading would
  ;; happen before the entry label.
  (:info label)
  (:temporary (:scs (any-reg) :from (:argument 0)) dst)
  (:temporary (:scs (any-reg) :from (:argument 1)) src)
  (:temporary (:scs (any-reg) :from (:argument 2)) num)
  (:temporary (:scs (descriptor-reg)) temp)
  (:results (new-start) (new-count))
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (emit-return-pc label)
    (note-this-location vop :non-local-entry)
    (let ((loop (gen-label))
	  (done (gen-label)))

      ;; Copy args.
      (load-stack-tn dst top)
      (move src start)
      (move num count)

      ;; Establish results.
      (sc-case new-start
	(any-reg (move new-start dst))
	(control-stack (store-stack-tn new-start dst)))
      (inst beq num zero-tn done)
      (sc-case new-count
	(any-reg (inst move new-count num))
	(control-stack (store-stack-tn new-count num)))

      ;; Copy stuff on stack.
      (emit-label loop)
      (loadw temp src)
      (inst addu src src vm:word-bytes)
      (storew temp dst)
      (inst addu num num (fixnumize -1))
      (inst bne num zero-tn loop)
      (inst addu dst dst vm:word-bytes)

      (emit-label done)
      (inst move csp-tn dst))))


;;; This VOP is just to force the TNs used in the cleanup onto the stack.
;;;
(define-vop (uwp-entry)
  (:info label)
  (:save-p :force-to-stack)
  (:results (block) (start) (count))
  (:ignore block start count)
  (:vop-var vop)
  (:generator 0
    (emit-return-pc label)
    (note-this-location vop :non-local-entry)))
