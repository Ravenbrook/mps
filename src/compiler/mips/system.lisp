;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/mips/system.lisp,v 1.49 1994/10/31 04:44:16 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;    MIPS VM definitions of various system hacking operations.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Mips conversion by William Lott and Christopher Hoover.
;;;
(in-package "MIPS")


;;;; Random pointer comparison VOPs

(define-vop (pointer-compare)
  (:args (x :scs (sap-reg))
	 (y :scs (sap-reg)))
  (:arg-types system-area-pointer system-area-pointer)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:note "inline comparison")
  (:variant-vars condition)
  (:generator 3
    (three-way-comparison x y condition :unsigned not-p target temp)))

(macrolet ((frob (name cond)
	     `(progn
		(def-primitive-translator ,name (x y) `(,',name ,x ,y))
		(defknown ,name (t t) boolean (movable foldable flushable))
		(define-vop (,name pointer-compare)
		  (:translate ,name)
		  (:variant ,cond)))))
  (frob pointer< :lt)
  (frob pointer> :gt))



;;;; Type frobbing VOPs

(define-vop (get-lowtag)
  (:translate get-lowtag)
  (:policy :fast-safe)
  (:args (object :scs (any-reg descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst and result object lowtag-mask)))

(define-vop (get-type)
  (:translate get-type)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    ;; Pick off objects with headers.
    (inst and ndescr object lowtag-mask)
    (inst xor ndescr other-pointer-type)
    (inst beq ndescr other-ptr)
    (inst xor ndescr (logxor other-pointer-type function-pointer-type))
    (inst beq ndescr function-ptr)

    ;; Pick off fixnums.
    (inst and result object 3)
    (inst beq result done)

    ;; Pick off structure and list pointers.
    (inst and result object 1)
    (inst bne result lowtag-only)
    (inst nop)

      ;; Must be an other immediate.
    (inst b done)
    (inst and result object type-mask)

    FUNCTION-PTR
    (load-type result object (- function-pointer-type))
    (inst b done)
    (inst nop)

    LOWTAG-ONLY
    (inst b done)
    (inst and result object lowtag-mask)

    OTHER-PTR
    (load-type result object (- other-pointer-type))
    (inst nop)
      
    DONE))

(define-vop (function-subtype)
  (:translate function-subtype)
  (:policy :fast-safe)
  (:args (function :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (load-type result function (- function-pointer-type))
    (inst nop)))

(define-vop (set-function-subtype)
  (:translate (setf function-subtype))
  (:policy :fast-safe)
  (:args (type :scs (unsigned-reg) :target result)
	 (function :scs (descriptor-reg)))
  (:arg-types positive-fixnum *)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (inst sb type function (- function-pointer-type))
    (move result type)))


(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 other-pointer-type)
    (inst srl res res type-bits)))

(define-vop (get-closure-length)
  (:translate get-closure-length)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 function-pointer-type)
    (inst srl res res type-bits)))

(define-vop (set-header-data)
  (:translate set-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg) :target res)
	 (data :scs (any-reg immediate zero)))
  (:arg-types * positive-fixnum)
  (:results (res :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) t1 t2)
  (:generator 6
    (loadw t1 x 0 other-pointer-type)
    (inst and t1 type-mask)
    (sc-case data
      (any-reg
       (inst sll t2 data (- type-bits 2))
       (inst or t1 t2))
      (immediate
       (inst or t1 (ash (tn-value data) type-bits)))
      (zero))
    (storew t1 x 0 other-pointer-type)
    (move res x)))

(define-vop (c::make-fixnum)
  (:args (ptr :scs (any-reg descriptor-reg)))
  (:results (res :scs (any-reg descriptor-reg)))
  (:generator 1
    ;;
    ;; Some code (the hash table code) depends on this returning a
    ;; positive number so make sure it does.
    (inst sll res ptr 3)
    (inst srl res res 1)))

(define-vop (c::make-other-immediate-type)
  (:args (val :scs (any-reg descriptor-reg))
	 (type :scs (any-reg descriptor-reg immediate)
	       :target temp))
  (:results (res :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 2
    (sc-case type
      ((immediate)
       (inst sll temp val type-bits)
       (inst or res temp (tn-value type)))
      (t
       (inst sra temp type 2)
       (inst sll res val (- type-bits 2))
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
    (loadw ndescr code 0 other-pointer-type)
    (inst srl ndescr type-bits)
    (inst sll ndescr word-shift)
    (inst subu ndescr other-pointer-type)
    (inst addu sap code ndescr)))

(define-vop (compute-function)
  (:args (code :scs (descriptor-reg))
	 (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    (loadw ndescr code 0 other-pointer-type)
    (inst srl ndescr type-bits)
    (inst sll ndescr word-shift)
    (inst addu ndescr offset)
    (inst addu ndescr (- function-pointer-type other-pointer-type))
    (inst addu func code ndescr)))

#+gengc
(progn

(define-vop (%function-self)
  (:policy :fast-safe)
  (:translate %function-self)
  (:args (function :scs (descriptor-reg)))
  (:temporary (:scs (any-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 3
    (loadw temp function function-entry-point-slot function-pointer-type)
    (inst addu result temp
	  (- function-pointer-type (* function-code-offset word-bytes)))))

(def-source-transform %closure-function (closure)
  `(%function-self ,closure))

(def-source-transform %funcallable-instance-function (fin)
  `(%function-self ,fin))

(define-vop (%set-function-self)
  (:policy :fast-safe)
  (:translate (setf %function-self))
  (:args (new-self :scs (descriptor-reg) :target result)
	 (function :scs (descriptor-reg)))
  (:temporary (:scs (any-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 3
    (inst addu temp new-self
	  (- (* function-code-offset word-bytes) function-pointer-type))
    (storew temp function function-entry-point-slot function-pointer-type)
    (move result new-self)))

;; Would have really liked to use a source-transform for this, but they
;; don't work with setf functions.
;; 
(defknown ((setf %funcallable-instance-function)) (function function) function
  (unsafe))
(deftransform (setf %funcallable-instance-function) ((value fin))
  '(setf (%function-self fin) value))

); #+gengc progn


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
      (inst lw count count-vector offset)
      (inst nop)
      (inst addu count 1)
      (inst sw count count-vector offset))))
