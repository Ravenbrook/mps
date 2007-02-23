;;; -*- Package: PPC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/ppc/system.lisp,v 1.3 2004/10/09 02:47:45 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    PPC VM definitions of various system hacking operations.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Mips conversion by William Lott and Christopher Hoover.
;;;
(in-package "PPC")



;;;; Type frobbing VOPs

(define-vop (get-lowtag)
  (:translate get-lowtag)
  (:policy :fast-safe)
  (:args (object :scs (any-reg descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst andi. result object vm:lowtag-mask)))

(define-vop (get-type)
  (:translate get-type)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 1)))
  (:results (result :scs (unsigned-reg) :from (:eval 0)))
  (:result-types positive-fixnum)
  (:generator 6
    ;; Grab the lowtag.
    (inst andi. result object lowtag-mask)
    ;; Check for various pointer types.
    (inst cmpwi result list-pointer-type)
    (inst beq done)
    (inst cmpwi result other-pointer-type)
    (inst beq other-pointer)
    (inst cmpwi result function-pointer-type)
    (inst beq function-pointer)
    (inst cmpwi result instance-pointer-type)
    (inst beq done)
    ;; Okay, it is an immediate.  If fixnum, we want zero.  Otherwise,
    ;; we want the low 8 bits.
    (inst andi. result object #b11)
    (inst beq done)
    ;; It wasn't a fixnum, so get the low 8 bits.
    (inst andi. result object type-mask)
    (inst b done)
    
    FUNCTION-POINTER
    (load-type result object (- function-pointer-type))
    (inst b done)

    OTHER-POINTER
    (load-type result object (- other-pointer-type))

    DONE))


(define-vop (function-subtype)
  (:translate function-subtype)
  (:policy :fast-safe)
  (:args (function :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (load-type result function (- vm:function-pointer-type))))

(define-vop (set-function-subtype)
  (:translate (setf function-subtype))
  (:policy :fast-safe)
  (:args (type :scs (unsigned-reg) :target result)
	 (function :scs (descriptor-reg)))
  (:arg-types positive-fixnum *)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (inst stb type function (- 3 function-pointer-type))
    (move result type)))

(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 vm:other-pointer-type)
    (inst srwi res res vm:type-bits)))

(define-vop (get-closure-length)
  (:translate get-closure-length)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 vm:function-pointer-type)
    (inst srwi res res vm:type-bits)))

(define-vop (set-header-data)
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target res)
	 (data :scs (any-reg immediate zero)))
  (:arg-types * positive-fixnum)
  (:results (res :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) t1 t2)
  (:generator 6
    (loadw t1 x 0 vm:other-pointer-type)
    (inst andi. t1 t1 vm:type-mask)
    (sc-case data
      (any-reg
       (inst slwi t2 data (- vm:type-bits 2))
       (inst or t1 t1 t2))
      (immediate
       (inst ori t1 t1 (ash (tn-value data) vm:type-bits)))
      (zero))
    (storew t1 x 0 vm:other-pointer-type)
    (move res x)))


(define-vop (make-fixnum)
  (:args (ptr :scs (any-reg descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    ;;
    ;; Some code (the hash table code) depends on this returning a
    ;; positive number so make sure it does.
    (inst slwi res ptr 3)
    (inst srwi res res 1)))

(define-vop (make-other-immediate-type)
  (:args (val :scs (any-reg descriptor-reg))
	 (type :scs (any-reg descriptor-reg immediate)
	       :target temp))
  (:results (res :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 2
    (sc-case type
      (immediate
       (inst slwi temp val vm:type-bits)
       (inst ori res temp (tn-value type)))
      (t
       (inst srawi temp type 2)
       (inst slwi res val (- vm:type-bits 2))
       (inst or res res temp)))))


;;;; Allocation

(define-vop (dynamic-space-free-pointer)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate dynamic-space-free-pointer)
  (:policy :fast-safe)
  (:generator 1
    (move int alloc-tn)))

(define-vop (binding-stack-pointer-sap)
  (:results (int :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate binding-stack-pointer-sap)
  (:policy :fast-safe)
  (:generator 1
    (move int bsp-tn)))

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
  (:args (code :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 10
    (loadw ndescr code 0 vm:other-pointer-type)
    (inst srwi ndescr ndescr vm:type-bits)
    (inst slwi ndescr ndescr vm:word-shift)
    (inst subi ndescr ndescr vm:other-pointer-type)
    (inst add sap code ndescr)))

(define-vop (compute-function)
  (:args (code :scs (descriptor-reg))
	 (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    (loadw ndescr code 0 vm:other-pointer-type)
    (inst srwi ndescr ndescr vm:type-bits)
    (inst slwi ndescr ndescr vm:word-shift)
    (inst add ndescr ndescr offset)
    (inst addi ndescr ndescr (- vm:function-pointer-type vm:other-pointer-type))
    (inst add func code ndescr)))



;;;; Other random VOPs.


(defknown unix::do-pending-interrupt () (values))
(define-vop (unix::do-pending-interrupt)
  (:policy :fast-safe)
  (:translate unix::do-pending-interrupt)
  (:generator 1
    (inst unimp pending-interrupt-trap)))


(define-vop (halt)
  (:generator 1
    (inst unimp halt-trap)))



;;;; Dynamic vop count collection support

(define-vop (count-me)
  (:args (count-vector :scs (descriptor-reg)))
  (:info index)
  (:temporary (:scs (non-descriptor-reg)) count)
  (:generator 1
    (let ((offset
	   (- (* (+ index vector-data-offset) word-bytes) other-pointer-type)))
      (assert (typep offset '(signed-byte 16)))
      (inst lwz count count-vector offset)
      (inst addi count count 1)
      (inst stw count count-vector offset))))

(defknown read-time-base ()
  (values (unsigned-byte 32) (unsigned-byte 32)))

(define-vop (read-time-base)
  (:translate read-time-base)
  (:args)
  (:policy :fast-safe)
  (:results (lo :scs (unsigned-reg))
	    (hi :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg) temp)
  (:temporary (:sc unsigned-reg :target hi) temp-hi)
  (:temporary (:sc unsigned-reg :target lo) temp-lo)
  (:generator 7
    ;; From 2.2.1
    (let ((loop (gen-label)))
      (emit-label loop)
      (inst mftbu temp-hi)
      (inst mftb temp-lo)
      (inst mftbu temp)
      (inst cmpw temp temp-hi)
      (inst bne loop)
      (move hi temp-hi)
      (move lo temp-lo))))

(defun read-cycle-counter ()
  (read-time-base))