;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package: x86 -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
 "$Header: /project/cmucl/cvsroot/src/compiler/x86/memory.lisp,v 1.9 2003/08/03 11:27:45 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the x86 definitions of some general purpose memory
;;; reference VOPs inherited by basic memory reference operations.
;;;
;;; Written by William Lott.
;;;
;;; Debugged by Paul F. Werkowski Spring/Summer 1995.
;;; Enhancements/debugging by Douglas T. Crosher 1996,1997,1999.
;;; 

(in-package :x86)

;;; Cell-Ref and Cell-Set are used to define VOPs like CAR, where the offset to
;;; be read or written is a property of the VOP used.  Cell-Setf is similar to
;;; Cell-Set, but delivers the new value as the result.  Cell-Setf-Function
;;; takes its arguments as if it were a setf function (new value first, as
;;; apposed to a setf macro, which takes the new value last).
;;;
(define-vop (cell-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (loadw value object offset lowtag)))
;;;
(define-vop (cell-set)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (storew value object offset lowtag)))
;;;
(define-vop (cell-setf)
  (:args (object :scs (descriptor-reg))
	 (value :scs (descriptor-reg any-reg) :target result))
  (:results (result :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (storew value object offset lowtag)
    (move result value)))
;;;
(define-vop (cell-setf-function)
  (:args (value :scs (descriptor-reg any-reg) :target result)
	 (object :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (storew value object offset lowtag)
    (move result value)))

;;; Define-Cell-Accessors  --  Interface
;;;
;;;    Define accessor VOPs for some cells in an object.  If the operation name
;;; is NIL, then that operation isn't defined.  If the translate function is
;;; null, then we don't define a translation.
;;;
(defmacro define-cell-accessors (offset lowtag
					ref-op ref-trans set-op set-trans)
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

(define-vop (cell-set-conditional)
  (:args (object :scs (descriptor-reg) :to :eval)
	 (old-value :scs (descriptor-reg any-reg) :target eax)
	 (new-value :scs (descriptor-reg any-reg)))
  (:temporary (:sc descriptor-reg :offset eax-offset
		   :from (:argument 1) :to :result :target result)  eax)
  (:variant-vars offset lowtag)
  (:results (result :scs (descriptor-reg any-reg)))
  (:guard (backend-featurep :i486))
  (:generator 4
    (move eax old-value)
    (inst cmpxchg (make-ea :dword :base object
			   :disp (- (* offset word-bytes) lowtag))
	  new-value)
    (move result eax)))

;;; X86 special
(define-vop (cell-xadd)
  (:args (object :scs (descriptor-reg) :to :result)
	 (value :scs (any-reg) :target result))
  (:results (result :scs (any-reg) :from (:argument 1)))
  (:result-types tagged-num)
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (move result value)
    (inst xadd (make-ea :dword :base object
			:disp (- (* offset word-bytes) lowtag))
	  value)))

;;; Slot-Ref and Slot-Set are used to define VOPs like Closure-Ref, where the
;;; offset is constant at compile time, but varies for different uses.
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
	 (value :scs (descriptor-reg any-reg immediate)))
  (:variant-vars base lowtag)
  (:info offset)
  (:generator 4
     (if (sc-is value immediate)
	 (let ((val (tn-value value)))
           (etypecase val
	     (integer
	      (inst mov
		    (make-ea :dword :base object
			     :disp (- (* (+ base offset) word-bytes) lowtag))
		    (fixnumize val)))
	     (symbol
	      (inst mov
		    (make-ea :dword :base object
			     :disp (- (* (+ base offset) word-bytes) lowtag))
		    (+ nil-value (static-symbol-offset val))))
	     (character
	      (inst mov
		    (make-ea :dword :base object
			     :disp (- (* (+ base offset) word-bytes) lowtag))
		    (logior (ash (char-code val) type-bits)
			    base-char-type)))))
	 ;; Else, value not immediate.
	 (storew value object (+ base offset) lowtag))))

(define-vop (slot-set-conditional)
  (:args (object :scs (descriptor-reg) :to :eval)
	 (old-value :scs (descriptor-reg any-reg) :target eax)
	 (new-value :scs (descriptor-reg any-reg)))
  (:temporary (:sc descriptor-reg :offset eax-offset
		   :from (:argument 1) :to :result :target result)  eax)
  (:variant-vars base lowtag)
  (:results (result :scs (descriptor-reg any-reg)))
  (:info offset)
  (:guard (backend-featurep :i486))
  (:generator 4
    (move eax old-value)
    (inst cmpxchg (make-ea :dword :base object
			   :disp (- (* (+ base offset) word-bytes) lowtag))
	  new-value)
    (move result eax)))

;;; X86 special
(define-vop (slot-xadd)
  (:args (object :scs (descriptor-reg) :to :result)
	 (value :scs (any-reg) :target result))
  (:results (result :scs (any-reg) :from (:argument 1)))
  (:result-types tagged-num)
  (:variant-vars base lowtag)
  (:info offset)
  (:generator 4
    (move result value)
    (inst xadd (make-ea :dword :base object
			:disp (- (* (+ base offset) word-bytes) lowtag))
	  value)))
