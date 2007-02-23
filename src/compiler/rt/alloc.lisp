;;; -*- Package: rt -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/alloc.lisp,v 1.6 2003/08/03 11:27:47 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/rt/alloc.lisp,v 1.6 2003/08/03 11:27:47 gerd Exp $
;;;
;;; Allocation VOPs for the IBM RT port.
;;;
;;; Written by William Lott.
;;; Converted by Bill Chiles.
;;;

(in-package "RT")



;;;; LIST and LIST*

(define-vop (list-or-list*)
  (:args (things :more t))
  (:temporary (:scs (descriptor-reg) :type list) ptr)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :type list :to (:result 0) :target result)
	      res)
  (:temporary (:scs (non-descriptor-reg) :type random) ndescr)
  (:temporary (:scs (word-pointer-reg)) alloc)
  (:info num)
  (:results (result :scs (descriptor-reg)))
  (:variant-vars star)
  (:policy :safe)
  (:generator 0
    (cond ((zerop num)
	   (move result null-tn))
	  ((and star (= num 1))
	   (move result (tn-ref-tn things)))
	  (t
	   (macrolet
	       ((store-car (tn list &optional (slot cons-car-slot))
		  `(let ((reg
			  (sc-case ,tn
			    ((any-reg descriptor-reg) ,tn)
			    (null null-tn)
			    (control-stack
			     (load-stack-tn temp ,tn)
			     temp))))
		     (storew reg ,list ,slot list-pointer-type))))
	     (let ((cons-cells (if star (1- num) num)))
	       (pseudo-atomic (ndescr)
		 (load-symbol-value alloc *allocation-pointer*)
		 (inst cal res alloc list-pointer-type)
		 (inst cal alloc alloc (* (pad-data-block cons-size)
					  cons-cells))
		 (store-symbol-value alloc *allocation-pointer*)
		 (move ptr res)
		 (dotimes (i (1- cons-cells))
		   (store-car (tn-ref-tn things) ptr)
		   (setf things (tn-ref-across things))
		   (inst cal ptr ptr (pad-data-block cons-size))
		   (storew ptr ptr
			   (- cons-cdr-slot cons-size)
			   list-pointer-type))
		 (store-car (tn-ref-tn things) ptr)
		 (cond (star
			(setf things (tn-ref-across things))
			(store-car (tn-ref-tn things) ptr cons-cdr-slot))
		       (t
			(storew null-tn ptr
				cons-cdr-slot list-pointer-type)))
		 (assert (null (tn-ref-across things)))
		 (move result res))
	       (load-symbol-value ndescr *internal-gc-trigger*)
	       (inst tlt ndescr alloc)))))))

(define-vop (list list-or-list*)
  (:variant nil))

(define-vop (list* list-or-list*)
  (:variant t))



;;;; Special purpose inline allocators.

(define-vop (allocate-code-object)
  (:args (boxed-arg :scs (any-reg))
	 (unboxed-arg :scs (any-reg) :target unboxed))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:scs (word-pointer-reg)) alloc)
  (:temporary (:scs (any-reg) :from (:argument 0)) boxed)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) unboxed)
  (:generator 100
    (inst li ndescr (lognot lowtag-mask))
    (inst cal boxed boxed-arg (fixnumize (1+ vm:code-trace-table-offset-slot)))
    (inst n boxed ndescr)
    (move unboxed unboxed-arg)
    (inst sr unboxed word-shift)
    (inst a unboxed lowtag-mask)
    (inst n unboxed ndescr)
    (pseudo-atomic (ndescr)
      (load-symbol-value alloc *allocation-pointer*)
      (inst cal result alloc other-pointer-type)
      (inst cas alloc boxed alloc)
      (inst cas alloc unboxed alloc)
      (store-symbol-value alloc *allocation-pointer*)
      (move ndescr boxed)
      (inst sl ndescr (- type-bits word-shift))
      (inst oil ndescr code-header-type)
      (storew ndescr result 0 other-pointer-type)
      (storew unboxed result code-code-size-slot other-pointer-type)
      (storew null-tn result code-entry-points-slot other-pointer-type)
      (storew null-tn result code-debug-info-slot other-pointer-type))
    (load-symbol-value ndescr *internal-gc-trigger*)
    (inst tlt ndescr alloc)))

(define-vop (make-symbol)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:scs (sap-reg)) temp)
  (:temporary (:scs (word-pointer-reg)) alloc)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:policy :fast-safe)
  (:translate make-symbol)
  (:generator 37
    (with-fixed-allocation (result temp alloc symbol-header-type symbol-size)
      (inst li temp unbound-marker-type)
      (storew temp result symbol-value-slot other-pointer-type)
      (storew temp result symbol-function-slot other-pointer-type)
      (storew temp result symbol-setf-function-slot other-pointer-type)
      (inst cai temp (make-fixup "undefined_tramp" :foreign))
      (storew temp result symbol-raw-function-addr-slot
	      other-pointer-type)
      (storew null-tn result symbol-plist-slot other-pointer-type)
      (storew name result symbol-name-slot other-pointer-type)
      (storew null-tn result symbol-package-slot other-pointer-type))))
