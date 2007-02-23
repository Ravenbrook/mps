;;; -*- Package: PPC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/ppc/alloc.lisp,v 1.14 2006/01/18 15:21:26 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;; Allocation VOPs for the PPC port.
;;;
;;; Written by William Lott.
;;; 

(in-package "PPC")


;;;; Dynamic-Extent (not implemented).

(define-vop (%dynamic-extent-start)
  (:args (saved-stack-pointer :scs (any-reg)))
  (:results)
  (:policy :safe)
  (:generator 0))

(define-vop (%dynamic-extent-end)
  (:args (saved-stack-pointer :scs (any-reg)))
  (:results)
  (:policy :safe)
  (:generator 0))


;;;; LIST and LIST*

(define-vop (list-or-list*)
  (:args (things :more t))
  (:temporary (:scs (descriptor-reg) :type list) ptr)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :type list :to (:result 0) :target result)
	      res)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:temporary (:scs (non-descriptor-reg)) alloc-temp)
  (:info num dynamic-extent)
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
	       ((maybe-load (tn)
		  (once-only ((tn tn))
		    `(sc-case ,tn
		       ((any-reg descriptor-reg zero null)
			,tn)
		       (control-stack
			(load-stack-tn temp ,tn)
			temp)))))
	     (let* ((cons-cells (if star (1- num) num))
		    (alloc (* (pad-data-block cons-size) cons-cells)))
	       (pseudo-atomic (pa-flag)
		 (allocation res alloc list-pointer-type :temp-tn alloc-temp
			     :flag-tn pa-flag)
		 (move ptr res)
		 (dotimes (i (1- cons-cells))
		   (storew (maybe-load (tn-ref-tn things)) ptr
			   cons-car-slot list-pointer-type)
		   (setf things (tn-ref-across things))
		   (inst addi ptr ptr (pad-data-block cons-size))
		   (storew ptr ptr
			   (- cons-cdr-slot cons-size)
			   list-pointer-type))
		 (storew (maybe-load (tn-ref-tn things)) ptr
			 cons-car-slot list-pointer-type)
		 (storew (if star
			     (maybe-load (tn-ref-tn (tn-ref-across things)))
			     null-tn)
			 ptr cons-cdr-slot list-pointer-type))
	       (move result res)))))))

(define-vop (list list-or-list*)
  (:variant nil))

(define-vop (list* list-or-list*)
  (:variant t))


;;;; Special purpose inline allocators.

(define-vop (allocate-code-object)
  (:args (boxed-arg :scs (any-reg))
	 (unboxed-arg :scs (any-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:scs (non-descriptor-reg)) size)
  (:temporary (:scs (any-reg) :from (:argument 0)) boxed)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) unboxed)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:generator 100
    (inst addi boxed boxed-arg (fixnumize (1+ code-trace-table-offset-slot)))
    (inst clrrwi boxed boxed lowtag-bits)
    (inst srwi unboxed unboxed-arg word-shift)
    (inst addi unboxed unboxed lowtag-mask)
    (inst clrrwi unboxed unboxed lowtag-bits)
    (pseudo-atomic (pa-flag)
      ;; Note: we don't have to subtract off the 4 that was added by
      ;; pseudo-atomic, because oring in other-pointer-type just adds
      ;; it right back.
      (inst add size boxed unboxed)
      (allocation result size other-pointer-type :temp-tn ndescr :flag-tn pa-flag)
      (inst slwi ndescr boxed (- type-bits word-shift))
      (inst ori ndescr ndescr code-header-type)
      (storew ndescr result 0 other-pointer-type)
      (storew unboxed result code-code-size-slot other-pointer-type)
      (storew null-tn result code-entry-points-slot other-pointer-type)
      (storew null-tn result code-debug-info-slot other-pointer-type))))

(define-vop (make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:generator 37
    (with-fixed-allocation (result pa-flag temp fdefn-type fdefn-size)
      ;; For the linkage-table stuff, we need to look up the address
      ;; of undefined_tramp from the linkage table instead of using
      ;; the address directly.
      ()
      (inst lr temp  (make-fixup (extern-alien-name "undefined_tramp")
				 #-linkage-table :foreign
				 #+linkage-table :foreign-data))
      #+linkage-table
      (loadw temp temp)
      (storew name result fdefn-name-slot other-pointer-type)
      (storew null-tn result fdefn-function-slot other-pointer-type)
      (storew temp result fdefn-raw-addr-slot other-pointer-type))))


(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info length dynamic-extent)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:ignore dynamic-extent)
  (:generator 10
    (let ((size (+ length closure-info-offset)))
      (pseudo-atomic (pa-flag)
     
	(allocation result (pad-data-block size) function-pointer-type 
		    :temp-tn temp :flag-tn pa-flag)
	(inst lr temp (logior (ash (1- size) type-bits) closure-header-type))
	(storew temp result 0 function-pointer-type)))
    (storew function result closure-function-slot function-pointer-type)))

;;; The compiler likes to be able to directly make value cells.
;;; 
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (with-fixed-allocation
     (result pa-flag temp value-cell-header-type value-cell-size)
     (storew value result value-cell-value-slot other-pointer-type))))



;;;; Automatic allocators for primitive objects.

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1
    (inst li result unbound-marker-type)))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag dynamic-extent)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:generator 4
    (with-fixed-allocation (result pa-flag temp type words :lowtag lowtag)
      )))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (any-reg)) bytes)
  (:temporary (:scs (non-descriptor-reg)) header)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:generator 6
    (inst addi bytes extra (* (1+ words) word-bytes))
    (inst slwi header bytes (- type-bits 2))
    (inst addi header header (+ (ash -2 type-bits) type))
    (inst clrrwi bytes bytes lowtag-bits)
    (pseudo-atomic (pa-flag)
      (allocation result bytes lowtag :temp-tn temp :flag-tn pa-flag)
      (storew header result 0 lowtag))))
