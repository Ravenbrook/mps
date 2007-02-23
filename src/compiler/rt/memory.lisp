;;; -*- Package: RT; Log: c.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/memory.lisp,v 1.2 1994/10/31 04:45:41 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/rt/memory.lisp,v 1.2 1994/10/31 04:45:41 ram Exp $
;;;
;;; This file contains the IBM RT definitions of some general purpose memory
;;; reference VOPs inherited by basic memory reference operations.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted by Bill Chiles.
;;;

(in-package "RT")


;;; CELL-REF -- VOP.
;;; CELL-SET -- VOP.
;;; CELL-SETF -- VOP.
;;; CELL-SETF-FUNCTION -- VOP.
;;;
;;; CELL-REF and CELL-SET are used to define VOPs like CAR, where the offset to
;;; be read or written is a property of the VOP used.  CELL-SETF is similar to
;;; CELL-SET, but delivers the new value as the result.  CELL-SETF-FUNCTION
;;; takes its arguments as if it were a setf function (new value first, as
;;; apposed to a setf macro, which takes the new value last).
;;;
(define-vop (cell-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (word-pointer-reg descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (loadw value object offset lowtag)))
;;;
(define-vop (cell-set)
  (:args (object :scs (descriptor-reg))
         (value :scs (word-pointer-reg descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (storew value object offset lowtag)))
;;;
(define-vop (cell-setf)
  (:args (object :scs (descriptor-reg))
	 (value :scs (word-pointer-reg descriptor-reg any-reg)
		:target result))
  (:results (result :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (storew value object offset lowtag)
    (move result value)))
;;;
(define-vop (cell-setf-function)
  (:args (value :scs (word-pointer-reg descriptor-reg any-reg)
		:target result)
	 (object :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (storew value object offset lowtag)
    (move result value)))

;;; DEFINE-CELL-ACCESSORS  --  Interface.
;;;
;;; Define accessor VOPs for some cells in an object.  If the operation name is
;;; NIL, then that operation isn't defined.  If the translate function is null,
;;; then we don't define a translation.
;;;
(defmacro define-cell-accessors (offset lowtag ref-op ref-trans set-op set-trans)
  `(progn
     ,@(when ref-op
	 `((define-vop (,ref-op cell-ref)
	     (:variant ,offset ,lowtag)
	     ,@(when ref-trans
		 `((:translate ,ref-trans))))))
     ,@(when set-op
	 `((define-vop (,set-op cell-setf)
	     (:variant ,offset ,lowtag)
	     ,@(when set-trans
		 `((:translate ,set-trans))))))))


;;; SLOT-REF -- VOP.
;;; SLOT-SET -- VOP.
;;;
;;; SLOT-REF and SLOT-SET are used to define VOPs like CLOSURE-REF, where the
;;; offset is constant at compile time, but varies for different uses.  We add
;;; in the stardard g-vector overhead.
;;;
(define-vop (slot-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars base lowtag)
  (:info offset)
  (:generator 4
    (loadw value object (+ base offset) lowtag)))
;;;
(define-vop (slot-set)
  (:args (object :scs (descriptor-reg))
	 (value :scs (descriptor-reg any-reg)))
  (:variant-vars base lowtag)
  (:info offset)
  (:generator 4
    (storew value object (+ base offset) lowtag)))



;;;; Indexed references:

(eval-when (compile eval)

;;; DEFINE-INDEXER  --  Internal.
;;;
;;; Define some VOPs for indexed memory reference.  Unless the index is
;;; constant, we must compute an intermediate result in a boxed temporary,
;;; since the RT doesn't have any indexed addressing modes.
;;;
(defmacro define-indexer (name write-p op shift &key gross-hack)
  `(define-vop (,name)
     (:args (object :scs (descriptor-reg) :to :eval)
	    (index :scs (any-reg immediate)
		   ,@(unless (zerop shift) '(:target temp)))
	    ,@(when write-p
		'((value :scs (any-reg descriptor-reg) :target result))))
     (:arg-types * tagged-num ,@(when write-p '(*)))
     (:temporary (:scs (interior-reg) :type interior) lip)
     ,@(unless (zerop shift)
	 `((:temporary (:scs (non-descriptor-reg)
			     :type random :from (:argument 1))
		       temp)))
     (:results (,(if write-p 'result 'value)
		:scs (any-reg descriptor-reg)))
     (:result-types *)
     (:variant-vars offset lowtag)
     (:policy :fast-safe)
     (:generator 5
       (sc-case index
	 ((immediate)
	  (inst ,op value object
		(- (+ (if (and (sc-is index immediate) (zerop (tn-value index)))
			  0
			  (ash (tn-value index) (- word-shift ,shift)))
		      (ash offset word-shift))
		   lowtag))
	  ,@(if write-p
		'((move result value))))
	 (t
	  ,@(if (zerop shift)
		;; Object must be the last arg to CAS here since it is cannot
		;; be in R0.
		`((inst cas lip index object))
		`((move temp index)
		  (inst sr temp ,shift)
		  (inst cas lip temp object)))
	  (inst ,op value lip (- (ash offset word-shift) lowtag))
	  ,@(if write-p
		'((move result value)))))
       ;; The RT lacks a signed-byte load instruction, so we have to sign
       ;; extend this case explicitly.  This is gross but obvious and easy.
       ,@(when gross-hack
	   '((inst sl value 24)
	     (inst sar value 24))))))

) ;EVAL-WHEN

(define-indexer word-index-ref nil l 0)
(define-indexer word-index-set t st 0)
(define-indexer halfword-index-ref nil lh 1)
(define-indexer signed-halfword-index-ref nil lha 1)
(define-indexer halfword-index-set t sth 1)
(define-indexer byte-index-ref nil lc 2)
(define-indexer signed-byte-index-ref nil lc 2 :gross-hack t)
(define-indexer byte-index-set t stc 2)
