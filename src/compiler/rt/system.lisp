;;; -*- Package: rt; Log: c.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/system.lisp,v 1.8 1994/10/31 04:45:41 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/rt/system.lisp,v 1.8 1994/10/31 04:45:41 ram Exp $
;;;
;;; IBM RT VM definitions of various system hacking operations.
;;;
;;; Written by Rob MacLachlan
;;;
;;; IBM RT conversion by Bill Chiles.
;;;

(in-package "RT")



;;;; Type frobbing VOPs.

(define-vop (get-lowtag)
  (:translate get-lowtag)
  (:policy :fast-safe)
  (:args (object :scs (any-reg descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst nilz result object lowtag-mask)))

(define-vop (get-type)
  (:translate get-type)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (let ((other-ptr (gen-label))
	  (function-ptr (gen-label))
	  (lowtag-only (gen-label))
	  (done (gen-label)))
      (test-type object ndescr other-ptr nil other-pointer-type)
      (test-type object ndescr function-ptr nil function-pointer-type)
      (test-type object ndescr lowtag-only nil
		 even-fixnum-type odd-fixnum-type list-pointer-type
		 structure-pointer-type)
      (inst bx done)
      (inst nilz result object type-mask)

      (emit-label function-ptr)
      (load-type result object function-pointer-type)
      (inst b done)

      (emit-label lowtag-only)
      (inst bx done)
      (inst nilz result object lowtag-mask)

      (emit-label other-ptr)
      (load-type result object other-pointer-type)
      
      (emit-label done))))

(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 7
    (loadw res x 0 other-pointer-type)
    (inst sr res type-bits)))

(define-vop (get-closure-length)
  (:translate get-closure-length)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 7
    (loadw res x 0 function-pointer-type)
    (inst sr res type-bits)))

;;; SET-HEADER-DATA -- VOP.
;;;
;;; In the immediate case for data, we use the OIL instruction assuming the
;;; data fits in the number of bits determined by 16 minus type-bits.  Due to
;;; known uses of this VOP, which only store single digit tags, the above
;;; assumption is reasonable, although unnecessarily slimy.
;;;
(define-vop (set-header-data)
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target res)
	 (data :scs (any-reg immediate) :target t2))
  (:arg-types * positive-fixnum)
  (:results (res :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg) :type random) t1)
  (:temporary (:scs (non-descriptor-reg) :type random :from (:argument 1)) t2)
  (:generator 15
    (loadw t1 x 0 other-pointer-type)
    (inst nilz t1 type-mask)
    (sc-case data
      (any-reg
       (move t2 data)
       ;; Since the data is in fixnum format, it is already shifted by 2 bits.
       (inst sl t2 (- type-bits 2))
       (inst o t1 t2))
      (immediate
       (let ((value (tn-value data)))
	 (unless (zerop value)
	   (inst oil t1 (ash value type-bits))))))
    (storew t1 x 0 other-pointer-type)
    (move res x)))

;;; MAKE-FIXNUM -- VOP.
;;;
;;; This is just used in hashing stuff.  It doesn't necessarily have to
;;; preserve all the bits in the pointer.  Some code expects a positive number,
;;; so make sure the right shift is logical.
;;;
(define-vop (make-fixnum)
  (:args (ptr :scs (any-reg descriptor-reg) :target temp))
  (:temporary (:scs (non-descriptor-reg) :from (:argument 0)) temp)
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 3
    (move temp ptr)
    (inst sl temp 3)
    (inst sr temp 1)
    (move res temp)))

(define-vop (make-other-immediate-type)
  (:args (val :scs (any-reg descriptor-reg))
	 (type :scs (any-reg descriptor-reg immediate)))
  (:results (res :scs (any-reg descriptor-reg) :from :load))
  (:temporary (:type random  :scs (non-descriptor-reg)) temp)
  (:generator 2
    (move res val)
    (inst sl res (- type-bits 2))
    (sc-case type
      (immediate
       (inst oil res (tn-value type)))
      (t
       ;; Type is a fixnum, so lose those lowtag bits.
       (move temp type)
       (inst sr temp 2)
       (inst o res temp)))))



;;;; Allocation

(define-vop (dynamic-space-free-pointer)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate dynamic-space-free-pointer)
  (:policy :fast-safe)
  (:generator 6
    (load-symbol-value int *allocation-pointer*)))

(define-vop (binding-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate binding-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 6
    (load-symbol-value int *binding-stack-pointer*)))

(define-vop (control-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate control-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move int csp-tn)))



;;;; Code object frobbing.

(define-vop (code-instructions)
  (:translate code-instructions)
  (:policy :fast-safe)
  (:args (code :scs (descriptor-reg) :target sap))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 10
    (loadw ndescr code 0 other-pointer-type)
    (inst sr ndescr type-bits)
    (inst sl ndescr word-shift)
    (inst s ndescr other-pointer-type)
    (move sap code)
    (inst a sap ndescr)))

(define-vop (compute-function)
  (:args (code :scs (descriptor-reg))
	 (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    (loadw ndescr code 0 other-pointer-type)
    (inst sr ndescr type-bits)
    (inst sl ndescr word-shift)
    (inst a ndescr offset)
    (inst a ndescr (- function-pointer-type other-pointer-type))
    (inst a ndescr code)
    (move func ndescr)))



;;;; Other random VOPs.


(defknown unix::do-pending-interrupt () (values))
(define-vop (unix::do-pending-interrupt)
  (:policy :fast-safe)
  (:translate unix::do-pending-interrupt)
  (:generator 1
    (inst break pending-interrupt-trap)))


(define-vop (halt)
  (:generator 1
    (inst break halt-trap)))



;;;; Dynamic vop count collection support

(define-vop (count-me)
  (:args (count-vector :scs (descriptor-reg)))
  (:info index)
  (:temporary (:scs (non-descriptor-reg)) count)
  (:generator 1
    (let ((offset
	   (- (* (+ index vector-data-offset) word-bytes) other-pointer-type)))
      (inst l count count-vector offset)
      (inst inc count 1)
      (inst st count count-vector offset))))
