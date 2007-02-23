;;; -*- Package: hppa -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/hppa/system.lisp,v 1.3 1994/10/31 04:42:45 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; HPPA VM definitions of various system hacking operations.
;;;
;;; Written by William Lott.
;;;
(in-package "HPPA")


;;;; Type frobbing VOPs

(define-vop (get-lowtag)
  (:translate get-lowtag)
  (:policy :fast-safe)
  (:args (object :scs (any-reg descriptor-reg) :target result))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst extru object 31 3 result)))

(define-vop (get-type)
  (:translate get-type)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 1)))
  (:results (result :scs (unsigned-reg) :from (:eval 0)))
  (:result-types positive-fixnum)
  (:generator 6
    (inst extru object 31 3 result)
    (inst comib := other-pointer-type result other-ptr :nullify t)
    (inst comib := function-pointer-type result function-ptr :nullify t)
    (inst bb t object 31 done :nullify t)
    (inst extru object 31 2 result :=)
    (inst extru object 31 8 result)
    (inst nop :tr)

    FUNCTION-PTR
    (load-type result object (- function-pointer-type))
    (inst nop :tr)
    
    OTHER-PTR
    (load-type result object (- other-pointer-type))
    
    DONE))

(define-vop (function-subtype)
  (:translate function-subtype)
  (:policy :fast-safe)
  (:args (function :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (load-type result function (- function-pointer-type))))

(define-vop (set-function-subtype)
  (:translate (setf function-subtype))
  (:policy :fast-safe)
  (:args (type :scs (unsigned-reg) :target result)
	 (function :scs (descriptor-reg)))
  (:arg-types positive-fixnum *)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (inst stb type (- 3 function-pointer-type) function)
    (move type result)))

(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 other-pointer-type)
    (inst srl res 8 res)))

(define-vop (get-closure-length)
  (:translate get-closure-length)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 function-pointer-type)
    (inst srl res 8 res)))

(define-vop (set-header-data)
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target res)
	 (data :scs (unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (res :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 6
    (loadw temp x 0 vm:other-pointer-type)
    (inst dep data 23 24 temp)
    (storew temp x 0 vm:other-pointer-type)
    (move x res)))

(define-vop (set-header-data-c)
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target res))
  (:arg-types * (:constant (signed-byte 5)))
  (:info data)
  (:results (res :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 5
    (loadw temp x 0 vm:other-pointer-type)
    (inst dep data 23 24 temp)
    (storew temp x 0 vm:other-pointer-type)
    (move x res)))

(define-vop (c::make-fixnum)
  (:args (ptr :scs (any-reg descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    ;;
    ;; Some code (the hash table code) depends on this returning a
    ;; positive number so make sure it does.
    (inst zdep ptr 29 29 res)))

(define-vop (c::make-other-immediate-type)
  (:args (val :scs (any-reg descriptor-reg))
	 (type :scs (any-reg descriptor-reg) :target temp))
  (:results (res :scs (any-reg descriptor-reg) :from (:argument 0)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 2
    (inst sll val (- type-bits 2) res)
    (inst sra type 2 temp)
    (inst or res temp res)))


;;;; Allocation

(define-vop (dynamic-space-free-pointer)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate dynamic-space-free-pointer)
  (:policy :fast-safe)
  (:generator 1
    (move alloc-tn int)))

(define-vop (binding-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate binding-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move bsp-tn int)))

(define-vop (control-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate control-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move csp-tn int)))


;;;; Code object frobbing.

(define-vop (code-instructions)
  (:translate code-instructions)
  (:policy :fast-safe)
  (:args (code :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 10
    (loadw ndescr code 0 other-pointer-type)
    (inst srl ndescr 8 ndescr)
    (inst sll ndescr 2 ndescr)
    (inst addi (- other-pointer-type) ndescr ndescr)
    (inst add code ndescr sap)))

(define-vop (compute-function)
  (:args (code :scs (descriptor-reg))
	 (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    (loadw ndescr code 0 vm:other-pointer-type)
    (inst srl ndescr 8 ndescr)
    (inst sll ndescr 2 ndescr)
    (inst add ndescr offset ndescr)
    (inst addi (- function-pointer-type other-pointer-type) ndescr ndescr)
    (inst add ndescr code func)))


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
      (inst ldw offset count-vector count)
      (inst addi 1 count count)
      (inst stw count offset count-vector))))
