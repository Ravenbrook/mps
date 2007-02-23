;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    This file contains the definitions of VOPs used for non-local exit
;;; (throw, lexical exit, etc.)
;;;
;;; Written by Rob MacLachlan
;;;
(in-package 'c)


;;; MAKE-NLX-SP-TN  --  Interface
;;;
;;;    Make an environment-live stack TN for saving the SP for NLX entry.
;;;
(defun make-nlx-sp-tn (env)
  (environment-live-tn
   (make-representation-tn *any-primitive-type* stack-arg-scn)
   env))
  

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
(defun make-dynamic-state-tns ()
  (make-n-tns 3 *any-primitive-type*))

(define-vop (save-dynamic-state)
  (:results (catch :scs (descriptor-reg))
	    (special :scs (descriptor-reg))
	    (eval :scs (descriptor-reg)))
  (:generator 13
    (load-global catch clc::current-catch-block)
    (inst lr special bs-tn)
    (inst cau eval zero-tn clc::t-16)
    (inst l eval eval (+ clc::*eval-stack-top*-offset clc::symbol-value))))

(define-vop (restore-dynamic-state one-arg-no-value-miscop)
  (:args (catch :scs (descriptor-reg))
	 (special :scs (descriptor-reg) :target a0)
	 (eval :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:variant-vars)
  (:generator 10
    (store-global catch clc::current-catch-block temp)
    (inst cau temp zero-tn clc::t-16)
    (inst st eval temp (+ clc::*eval-stack-top*-offset clc::symbol-value))
    (let ((skip (gen-label)))
      (inst c special bs-tn)
      (inst bb :eq skip)
      (unless (location= special a0)
	(inst lr a0 special))
      (inst miscop 'clc::unbind-to-here)
      (emit-label skip))))

(define-vop (current-stack-pointer)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    (inst lr res sp-tn)))


;;;; Unwind miscop VOPs:

(define-vop (unwind three-arg-no-value-miscop)
  (:variant 'clc::unwind)
  (:translate %continue-unwind))

(define-vop (throw three-arg-no-value-miscop)
  (:variant 'clc::throw))


;;;; Unwind block hackery:

;;; Compute the address of the catch block from its TN, then store into the
;;; block the current Fp, Env, Unwind-Protect, and the entry PC.
;;;
(define-vop (make-unwind-block)
  (:args (tn)
	 (entry-offset :scs (any-reg descriptor-reg)))
  (:results (block :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :target block) result)
  (:generator 22
    (inst ai result fp-tn (* (tn-offset tn) 4))
    (load-global temp clc::current-unwind-protect-block)
    (storew temp result system:%unwind-block-current-uwp)
    (storew fp-tn result system:%unwind-block-current-fp)
    (storew env-tn result system:%unwind-block-current-env)
    (storew entry-offset result system:%unwind-block-entry-pc)
    (unless (location= result block)
      (inst lr block result))))


;;; Like Make-Unwind-Block, except that we also store in the specified tag, and
;;; link the block into the Current-Catch list.
;;;
(define-vop (make-catch-block)
  (:args (tn)
	 (tag :scs (any-reg descriptor-reg))
	 (entry-offset :scs (any-reg descriptor-reg)))
  (:results (block :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :target block) result)
  (:generator 44
    (inst ai result fp-tn (* (tn-offset tn) 4))
    (load-global temp clc::current-unwind-protect-block)
    (storew temp result system:%unwind-block-current-uwp)
    (storew fp-tn result system:%unwind-block-current-fp)
    (storew env-tn result system:%unwind-block-current-env)
    (storew entry-offset result system:%unwind-block-entry-pc)

    (storew tag result system:%catch-block-tag)
    (load-global temp clc::current-catch-block)
    (storew temp result system:%catch-block-previous-catch)
    (store-global result clc::current-catch-block temp)
    
    (unless (location= result block)
      (inst lr block result))))


;;; Just set the current unwind-protect to TN's address.  This instantiates an
;;; unwind block as an unwind-protect.
;;;
(define-vop (set-unwind-protect)
  (:args (tn))
  (:temporary (:scs (descriptor-reg)) temp new-uwp)
  (:generator 7
    (inst ai new-uwp fp-tn (* (tn-offset tn) 4))
    (store-global new-uwp clc::current-unwind-protect-block temp)))


(define-vop (unlink-unwind-block)
  (:temporary (:scs (descriptor-reg)) block temp)
  (:variant-vars global slot)
  (:policy :fast-safe)
  (:generator 17
    (load-global block global)
    (loadw block block slot)
    (store-global block global temp)))

(define-vop (unlink-catch-block unlink-unwind-block)
  (:variant clc::current-catch-block system:%catch-block-previous-catch)
  (:translate %catch-breakup))

(define-vop (unlink-unwind-protect unlink-unwind-block)
  (:variant clc::current-unwind-protect-block system:%unwind-block-current-uwp)
  (:translate %unwind-protect-breakup))


;;;; NLX entry VOPs:
;;;
;;;    We can't just make these miscop variants, since they take funny wired
;;; operands.
;;;

(define-vop (nlx-entry two-arg-no-value-miscop)
  (:args (top :scs (descriptor-reg)
	      :target a0)
	 (start)
	 (count))
  (:results (values :more t))
  (:info nvals)
  (:ignore start count values nl0 nl1 a2 a3 misc-pc)
  (:variant-vars)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (note-this-location vop :non-local-exit)
    (unless (location= a0 top)
      (inst lr a0 top))
    (inst miscopx 'clc::nlx-entry-default-values)
    (inst cal a1 zero-tn nvals)))


(define-vop (nlx-entry-multiple two-arg-two-value-miscop)
  (:args (top :scs (descriptor-reg)
	      :target a0)
	 (start)
	 (count))
  (:ignore start count nl0 nl1 a2 a3 misc-pc)
  (:variant-vars)
  (:save-p :force-to-stack)
  (:vop-var vop)
  (:generator 30
    (note-this-location vop :non-local-exit)
    (unless (location= a0 top)
      (inst lr a0 top))
    (inst miscop 'clc::nlx-entry-receive-values)
    (unless (location= a0 r)
      (inst lr r a0))
    (unless (location= a1 r1)
      (inst lr r1 a1))))


;;; This VOP is just to force the TNs used in the cleanup onto the stack.
;;;
(define-vop (uwp-entry)
  (:save-p :force-to-stack)
  (:results (block) (start) (count))
  (:ignore block start count)
  (:vop-var vop)
  (:generator 0
    (note-this-location vop :non-local-exit)))
