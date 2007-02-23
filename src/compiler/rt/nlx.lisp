;;; -*- Package: RT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/nlx.lisp,v 1.6 2003/08/03 11:27:47 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/rt/nlx.lisp,v 1.6 2003/08/03 11:27:47 gerd Exp $
;;;
;;; This file contains the definitions of VOPs used for non-local exit (throw,
;;; lexical exit, etc.)
;;;
;;; Written by Rob MacLachlan
;;; Converted to IBM RT by William Lott and Bill Chiles.
;;;

(in-package "RT")


;;; MAKE-NLX-SP-TN  --  Interface.
;;;
;;; Make an environment-live stack TN for saving the SP for NLX entry.
;;;
(def-vm-support-routine make-nlx-sp-tn (env)
  (environment-live-tn
   (make-representation-tn *word-pointer-type*
			   (sc-number-or-lose 'word-pointer-reg *backend*))
   env))



;;; Save and restore dynamic environment.
;;;
;;;    These VOPs are used in the reentered function to restore the appropriate
;;; dynamic environment.  Currently we only save the Current-Catch and binding
;;; stack pointer.  We don't need to save/restore the current unwind-protect,
;;; since unwind-protects are implicitly processed during unwinding.  If there
;;; were any additional stacks, then this would be the place to restore the top
;;; pointers.


;;; MAKE-DYNAMIC-STATE-TNS  --  Interface.
;;;
;;; Return a list of TNs that can be used to snapshot the dynamic state for use
;;; with the Save/Restore-Dynamic-Environment VOPs.
;;;
(def-vm-support-routine make-dynamic-state-tns ()
  (make-n-tns 4 *fixnum-primitive-type*))

(define-vop (save-dynamic-state)
  (:results (catch :scs (any-reg))
	    (nfp :scs (any-reg))
	    (nsp :scs (any-reg))
	    (eval :scs (any-reg)))
  (:vop-var vop)
  (:generator 13
    (load-symbol-value catch lisp::*current-catch-block*)
    (let ((cur-nfp (current-nfp-tn vop)))
      (if cur-nfp
	  (move nfp cur-nfp)
	  (inst li nfp 0)))
    (move nsp nsp-tn)
    (load-symbol-value eval lisp::*eval-stack-top*)))

(define-vop (restore-dynamic-state)
  (:args (catch :scs (any-reg))
	 (nfp :scs (any-reg))
	 (nsp :scs (any-reg))
	 (eval :scs (any-reg)))
  (:temporary (:scs (descriptor-reg) :from (:eval 0)) symbol value)
  (:temporary (:scs (word-pointer-reg) :from (:eval 0)) bsp)
  (:vop-var vop)
  (:generator 10
    (store-symbol-value catch lisp::*current-catch-block*)
    (store-symbol-value eval lisp::*eval-stack-top*)
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(move cur-nfp nfp)))
    (move nsp-tn nsp)))

(define-vop (current-stack-pointer)
  (:results (res :scs (any-reg word-pointer-reg descriptor-reg)))
  (:generator 1
    (move res csp-tn)))

(define-vop (current-binding-pointer)
  (:results (res :scs (any-reg word-pointer-reg descriptor-reg)))
  (:generator 1
    (load-symbol-value res *binding-stack-pointer*)))



;;;; Unwind block hackery:

;;; MAKE-UNWIND-BLOCK -- VOP.
;;;
;;; Compute the address of the catch block from its TN, then store into the
;;; block the current Fp, Env, Unwind-Protect, and the entry PC.
;;;
(define-vop (make-unwind-block)
  (:args (tn))
  (:info entry-label)
  (:results (block :scs (word-pointer-reg)))
  (:temporary (:scs (word-pointer-reg) :target block) block-ptr)
  (:temporary (:scs (descriptor-reg)) temp)
  (:generator 22
    (inst cal block-ptr cfp-tn (* (tn-offset tn) vm:word-bytes))
    (load-symbol-value temp lisp::*current-unwind-protect-block*)
    (storew temp block-ptr vm:unwind-block-current-uwp-slot)
    (storew cfp-tn block-ptr vm:unwind-block-current-cont-slot)
    (storew code-tn block-ptr vm:unwind-block-current-code-slot)
    (inst compute-lra-from-code temp code-tn entry-label)
    (storew temp block-ptr vm:catch-block-entry-pc-slot)
    (move block block-ptr)))


;;; MAKE-CATCH-BLOCK -- VOP.
;;;
;;; Like MAKE-UNWIND-BLOCK, except that we also store in the specified tag, and
;;; link the block into the Current-Catch list.
;;;
(define-vop (make-catch-block)
  (:args (tn)
	 (tag :scs (descriptor-reg)))
  (:info entry-label)
  (:results (block :scs (word-pointer-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (word-pointer-reg) :target block :to (:result 0)) result)
  (:generator 44
    (inst cal result cfp-tn (* (tn-offset tn) vm:word-bytes))
    (load-symbol-value temp lisp::*current-unwind-protect-block*)
    (storew temp result vm:catch-block-current-uwp-slot)
    (storew cfp-tn result vm:catch-block-current-cont-slot)
    (storew code-tn result vm:catch-block-current-code-slot)
    (inst compute-lra-from-code temp code-tn entry-label)
    (storew temp result vm:catch-block-entry-pc-slot)

    (storew tag result vm:catch-block-tag-slot)
    (load-symbol-value temp lisp::*current-catch-block*)
    (storew temp result vm:catch-block-previous-catch-slot)
    (store-symbol-value result lisp::*current-catch-block*)

    (move block result)))


;;; SET-UNWIND-PROTECT -- VOP.
;;;
;;; Just set the current unwind-protect to TN's address.  This instantiates an
;;; unwind block as an unwind-protect.
;;;
(define-vop (set-unwind-protect)
  (:args (tn))
  (:temporary (:scs (descriptor-reg)) new-uwp)
  (:generator 7
    (inst cal new-uwp cfp-tn (* (tn-offset tn) vm:word-bytes))
    (store-symbol-value new-uwp lisp::*current-unwind-protect-block*)))

;;; UNLINK-CATCH-BLOCK -- VOP.
;;;
;;; Remove the catch block from the chain of catches.  This happens when
;;; we drop out of a catch instead of throwing.
;;; 
(define-vop (unlink-catch-block)
  (:temporary (:scs (word-pointer-reg)) block)
  (:policy :fast-safe)
  (:translate %catch-breakup)
  (:generator 17
    (load-symbol-value block lisp::*current-catch-block*)
    (loadw block block vm:catch-block-previous-catch-slot)
    (store-symbol-value block lisp::*current-catch-block*)))

;;; UNLINK-UNWIND-PROTECT -- VOP.
;;;
;;; Same thing with unwind protects.
;;; 
(define-vop (unlink-unwind-protect)
  (:temporary (:scs (word-pointer-reg)) block)
  (:policy :fast-safe)
  (:translate %unwind-protect-breakup)
  (:generator 17
    (load-symbol-value block lisp::*current-unwind-protect-block*)
    (loadw block block vm:unwind-block-current-uwp-slot)
    (store-symbol-value block lisp::*current-unwind-protect-block*)))


;;;; NLX entry VOPs:


;;; NLX-ENTRY -- VOP.
;;;
;;; We were just thrown to, so load up the results.
;;; 
(define-vop (nlx-entry)
  (:args (sp) ; Note: we can't list an sc-restriction, 'cause any load vops
	      ; would be inserted before the LRA.
	 (start)
	 (count))
  (:results (values :more t))
  (:temporary (:scs (descriptor-reg)) move-temp)
  (:info label nvals)
  (:save-p :force-to-stack)
  (:generator 30
    (emit-return-pc label)
    (cond ((zerop nvals))
	  ((= nvals 1)
	   (let ((no-values (gen-label)))
	     (inst c count 0)
	     (inst bcx :eq no-values)
	     (move (tn-ref-tn values) null-tn)
	     (loadw (tn-ref-tn values) start)
	     (emit-label no-values)))
	  (t
	   (collect ((defaults))
	     (inst c count 0)
	     (do ((i 0 (1+ i))
		  (tn-ref values (tn-ref-across tn-ref)))
		 ((null tn-ref))
	       (let ((default-lab (gen-label))
		     (tn (tn-ref-tn tn-ref)))
		 (defaults (cons default-lab tn))
		 
		 (inst bc :eq default-lab)
		 (inst s count (fixnumize 1))
		 (sc-case tn
		   ((descriptor-reg any-reg)
		    (loadw tn start i))
		   (control-stack
		    (loadw move-temp start i)
		    (store-stack-tn move-temp tn)))))
	     
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
			(store-stack-tn null-tn tn)))))
		 (inst b defaulting-done))))))
    (load-stack-tn csp-tn sp)))


(define-vop (nlx-entry-multiple)
  ;; Again, no SC restrictions for the args, 'cause the loading would
  ;; happen before the entry label.  But we know that start and count will
  ;; be in registers due to the way this vop is used.
  (:args (top :target dst)
	 (start :target src)
	 (count :target num))
  (:info label)
  (:temporary (:scs (any-reg) :from (:argument 0)) dst)
  (:temporary (:scs (any-reg) :from (:argument 1)) src)
  (:temporary (:scs (any-reg) :from (:argument 2)) num)
  (:temporary (:scs (descriptor-reg)) temp)
  (:results (new-start) (new-count))
  (:save-p :force-to-stack)
  (:generator 30
    (emit-return-pc label)
    (let ((loop (gen-label))
	  (done (gen-label)))

      ;; Copy args.
      (load-stack-tn dst top)
      (move src start)
      (inst a num count 0)

      ;; Establish results.
      (sc-case new-start
	((any-reg word-pointer-reg) (move new-start dst))
	(control-stack (store-stack-tn dst new-start)))
      (inst bcx :eq done)
      (sc-case new-count
	(any-reg (inst move new-count num))
	(control-stack (store-stack-tn num new-count)))

      ;; Copy stuff on stack.
      (emit-label loop)
      (loadw temp src)
      (inst inc src vm:word-bytes)
      (storew temp dst)
      (inst s num num (fixnumize 1))
      (inst bncx :eq loop)
      (inst inc dst vm:word-bytes)

      (emit-label done)
      (move csp-tn dst))))


;;; This VOP is just to force the TNs used in the cleanup onto the stack.
;;;
(define-vop (uwp-entry)
  (:info label)
  (:save-p :force-to-stack)
  (:results (block) (start) (count))
  (:ignore block start count)
  (:generator 0
    (emit-return-pc label)))
