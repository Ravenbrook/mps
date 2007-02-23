;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/mips/alloc.lisp,v 1.31 2003/08/25 20:50:59 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; Allocation VOPs for the MIPS port.
;;;
;;; Written by William Lott.
;;; 

(in-package "MIPS")


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

#-gengc
(define-vop (list-or-list*)
  (:args (things :more t))
  (:temporary (:scs (descriptor-reg) :type list) ptr)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :type list :to (:result 0) :target result)
	      res)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
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
	       ((store-car (tn list &optional (slot cons-car-slot))
		  `(let ((reg
			  (sc-case ,tn
			    ((any-reg descriptor-reg) ,tn)
			    (zero zero-tn)
			    (null null-tn)
			    (control-stack
			     (load-stack-tn temp ,tn)
			     temp))))
		     (storew reg ,list ,slot list-pointer-type))))
	     (let ((cons-cells (if star (1- num) num)))
	       (pseudo-atomic (pa-flag
			       :extra (* (pad-data-block cons-size)
					 cons-cells))
		 (inst or res alloc-tn list-pointer-type)
		 (move ptr res)
		 (dotimes (i (1- cons-cells))
		   (store-car (tn-ref-tn things) ptr)
		   (setf things (tn-ref-across things))
		   (inst addu ptr ptr (pad-data-block cons-size))
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
		 (move result res))))))))

#-gengc
(define-vop (list list-or-list*)
  (:variant nil))

#-gengc
(define-vop (list* list-or-list*)
  (:variant t))

#+gengc
(def-source-transform list (&rest args)
  (iterate repeat ((args args))
    (if args
	`(cons ,(car args) ,(repeat (cdr args)))
	nil)))

#+gengc
(def-source-transform list* (arg &rest others)
  (iterate repeat ((args (cons arg others)))
    (if (cdr args)
	`(cons ,(car args) ,(repeat (cdr args)))
	(car args))))


;;;; Special purpose inline allocators.

#-gengc
(define-vop (allocate-code-object)
  (:args (boxed-arg :scs (any-reg))
	 (unboxed-arg :scs (any-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:scs (any-reg) :from (:argument 0)) boxed)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) unboxed)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:generator 100
    (inst li ndescr (lognot lowtag-mask))
    (inst addu boxed boxed-arg
	  (fixnumize (1+ code-trace-table-offset-slot)))
    (inst and boxed ndescr)
    (inst srl unboxed unboxed-arg word-shift)
    (inst addu unboxed unboxed lowtag-mask)
    (inst and unboxed ndescr)
    (inst sll ndescr boxed (- type-bits word-shift))
    (inst or ndescr code-header-type)
    
    (pseudo-atomic (pa-flag)
      (inst or result alloc-tn other-pointer-type)
      (storew ndescr result 0 other-pointer-type)
      (storew unboxed result code-code-size-slot other-pointer-type)
      (storew null-tn result code-entry-points-slot other-pointer-type)
      (inst addu alloc-tn boxed)
      (inst addu alloc-tn unboxed))

    (storew null-tn result code-debug-info-slot other-pointer-type)))

#+gengc
(define-vop (allocate-code-object)
  (:args (boxed :scs (any-reg))
	 (unboxed :scs (any-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc descriptor-reg :offset a0-offset :from (:eval 0)
	       :to (:result 0) :target result)
	      a0)
  (:temporary (:sc any-reg :offset nargs-offset :to (:eval 1)) words)
  (:temporary (:sc non-descriptor-reg :offset nl0-offset :to (:eval 1)) lowtag)
  (:temporary (:sc any-reg :offset nl1-offset :to (:eval 1)) header)
  (:temporary (:sc any-reg :offset nl2-offset :to (:eval 1)) first-word)
  (:temporary (:sc any-reg :offset ra-offset :from (:eval 0) :to (:eval 1)) ra)
  (:ignore ra)
  (:generator 100
    (inst li lowtag (lognot lowtag-mask))
    (inst addu words boxed (fixnumize (1+ code-debug-info-slot)))
    (inst and words lowtag)
    (inst addu first-word unboxed (fixnumize 1))
    (inst and first-word lowtag)
    (inst sll header words (- type-bits 2))
    (inst or header code-header-type)
    (inst addu words first-word)
    (inst jal (make-fixup 'large-alloc :assembly-routine))
    (inst li lowtag other-pointer-type)
    (move result a0)
    (storew null-tn result code-entry-points-slot other-pointer-type)
    (storew null-tn result code-debug-info-slot other-pointer-type)))

(define-vop (make-fdefn)
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:scs (non-descriptor-reg)) temp)
  #-gengc (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:generator 37
    (with-fixed-allocation (result pa-flag temp fdefn-type fdefn-size)
      (storew name result fdefn-name-slot other-pointer-type)
      (storew null-tn result fdefn-function-slot other-pointer-type)
      (inst li temp (make-fixup "undefined_tramp" :foreign))
      (storew temp result fdefn-raw-addr-slot other-pointer-type))))

(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info length dynamic-extent)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  #+gengc
  (:ignore function length temp pa-flag result)
  #-gengc
  (:ignore dynamic-extent)
  (:generator 10
    #-gengc
    (let ((size (+ length closure-info-offset)))
      (inst li temp (logior (ash (1- size) type-bits) closure-header-type))
      (pseudo-atomic (pa-flag :extra (pad-data-block size))
	(inst or result alloc-tn function-pointer-type)
	(storew temp result 0 function-pointer-type))
      (storew function result closure-function-slot function-pointer-type))
    #+gengc
    (error "MAKE-CLOSURE vop used in gengc system?")))

;;; The compiler likes to be able to directly make value cells.
;;; 
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg null zero)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  #-gengc (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (with-fixed-allocation
	(result pa-flag temp value-cell-header-type value-cell-size))
    (storew value result value-cell-value-slot other-pointer-type)))


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
  #-gengc (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:generator 4
    #-gengc
    (pseudo-atomic (pa-flag :extra (pad-data-block words))
      (inst or result alloc-tn lowtag)
      (when type
	(inst li temp (logior (ash (1- words) type-bits) type))
	(storew temp result 0 lowtag)))
    #+gengc
    (progn
      (when type
	(inst li temp (logior (ash (1- words) type-bits) type)))
      (without-scheduling ()
	(inst or result alloc-tn lowtag)
	(when type
	  (storew temp alloc-tn))
	(inst addu alloc-tn (pad-data-block words))))))

#-gengc
(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (any-reg)) header)
  (:temporary (:scs (non-descriptor-reg)) bytes)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset) pa-flag)
  (:generator 6
    (inst addu bytes extra (* (1+ words) word-bytes))
    (inst sll header bytes (- type-bits 2))
    (inst addu header header (+ (ash -2 type-bits) type))
    (inst srl bytes bytes lowtag-bits)
    (inst sll bytes bytes lowtag-bits)
    (pseudo-atomic (pa-flag)
      (inst or result alloc-tn lowtag)
      (storew header result 0 lowtag)
      (inst addu alloc-tn alloc-tn bytes))))

#+gengc
(define-vop (var-alloc)
  (:args (extra :scs (any-reg) :target nargs))
  (:arg-types positive-fixnum)
  (:info name words type lowtag)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc non-descriptor-reg :offset nl0-offset) nl0)
  (:temporary (:sc non-descriptor-reg :offset nl1-offset) nl1)
  (:temporary (:sc non-descriptor-reg :offset nl2-offset) nl2)
  (:temporary (:sc any-reg :offset nargs-offset) nargs)
  (:temporary (:sc descriptor-reg :offset a0-offset :target result
	       :from (:argument 0) :to (:result 0))
	      a0)
  (:temporary (:sc any-reg :offset ra-offset :from (:eval 0) :to (:eval 1)) ra)
  (:ignore name nl2 ra)
  (:generator 10
    (inst addu nargs extra (fixnumize words))
    (inst sll nl1 nargs (- type-bits word-shift))
    (inst addu nl1 (+ (ash -1 type-bits) type))
    (inst jal (make-fixup 'var-alloc :assembly-routine))
    (inst li nl0 lowtag)
    (move result a0)))
