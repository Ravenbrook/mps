;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/sparc/system.lisp,v 1.17 2004/01/16 03:24:49 toy Exp $")
;;;
;;; **********************************************************************
;;;
;;;    MIPS VM definitions of various system hacking operations.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Mips conversion by William Lott and Christopher Hoover.
;;;
(in-package "SPARC")



;;;; Type frobbing VOPs

(define-vop (get-lowtag)
  (:translate get-lowtag)
  (:policy :fast-safe)
  (:args (object :scs (any-reg descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst and result object vm:lowtag-mask)))

(define-vop (get-type)
  (:translate get-type)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:eval 1)))
  (:results (result :scs (unsigned-reg) :from (:eval 0)))
  (:result-types positive-fixnum)
  (:generator 6
    ;; Grab the lowtag.
    (inst andcc result object lowtag-mask)
    ;; Check for various pointer types.
    (inst cmp result list-pointer-type)
    (inst b :eq done)
    (inst cmp result other-pointer-type)
    (inst b :eq other-pointer)
    (inst cmp result function-pointer-type)
    (inst b :eq function-pointer)
    (inst cmp result instance-pointer-type)
    (inst b :eq done)
    ;; Okay, it is an immediate.  If fixnum, we want zero.  Otherwise,
    ;; we want the low 8 bits.
    (inst andcc zero-tn object vm:fixnum-tag-mask)
    (inst b :eq done)
    (inst li result 0)			; Watch out!  LI in branch delay slot!
    ;; It wasn't a fixnum, so get the low 8 bits.
    (inst b done)
    (inst and result object type-mask)
    
    FUNCTION-POINTER
    (inst b done)
    (load-type result object (- function-pointer-type))

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
    ;; Sparc is big-endian, and the type bits are in the least
    ;; significant byte of the word, which means the type bits are at
    ;; the highest byte.
    (inst stb type function (- (- vm:word-bytes 1) function-pointer-type))
    (move result type)))

(define-vop (get-header-data)
  (:translate get-header-data)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 vm:other-pointer-type)
    (inst srln res res vm:type-bits)))

(define-vop (get-closure-length)
  (:translate get-closure-length)
  (:policy :fast-safe)
  (:args (x :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 6
    (loadw res x 0 vm:function-pointer-type)
    (inst srln res res vm:type-bits)))

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
    (inst and t1 vm:type-mask)
    (sc-case data
      (any-reg
       (inst slln t2 data (- vm:type-bits vm:fixnum-tag-bits))
       (inst or t1 t2))
      (immediate
       (inst or t1 (ash (tn-value data) vm:type-bits)))
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
    (inst slln res ptr vm:lowtag-bits)
    (inst srln res res 1)))

(define-vop (make-other-immediate-type)
  (:args (val :scs (any-reg descriptor-reg))
	 (type :scs (any-reg descriptor-reg immediate)
	       :target temp))
  (:results (res :scs (any-reg descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 2
    (sc-case type
      (immediate
       (inst slln temp val vm:type-bits)
       (inst or res temp (tn-value type)))
      (t
       (inst sran temp type vm:fixnum-tag-bits)
       (inst slln res val (- vm:type-bits vm:fixnum-tag-bits))
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
    (inst srln ndescr vm:type-bits)
    (inst slln ndescr vm:word-shift)
    (inst sub ndescr vm:other-pointer-type)
    (inst add sap code ndescr)))

(define-vop (compute-function)
  (:args (code :scs (descriptor-reg))
	 (offset :scs (signed-reg unsigned-reg)))
  (:arg-types * positive-fixnum)
  (:results (func :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:generator 10
    (loadw ndescr code 0 vm:other-pointer-type)
    (inst srln ndescr vm:type-bits)
    (inst slln ndescr vm:word-shift)
    (inst add ndescr offset)
    (inst add ndescr (- vm:function-pointer-type vm:other-pointer-type))
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
      (assert (typep offset '(signed-byte 13)))
      (inst ldn count count-vector offset)
      (inst add count 1)
      (inst stn count count-vector offset))))

;; The RDTICK instruction on Sparc V9s allows access to a 63-bit cycle
;; counter.

(defknown read-cycle-counter ()
  (values (unsigned-byte 32) (unsigned-byte 32)))

;; Note: This should probably really only be used sparc-v8plus because
;; the tick counter is returned in a 64-bit register.
(define-vop (read-cycle-counter)
  (:translate read-cycle-counter)
  (:guard (backend-featurep :sparc-v9))
  (:args)
  (:policy :fast-safe)
  (:results (lo :scs (unsigned-reg))
	    (hi :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  ;; The temporary can be any of the 64-bit non-descriptor regs.  It
  ;; can't be any non-descriptor reg because they might not get saved
  ;; on a task switch, since we're still a 32-bit app.  Arbitrarily
  ;; select nl5.
  (:temporary (:sc unsigned-reg :offset nl5-offset) tick)
  (:generator 3
    (inst rdtick tick)
    ;; Get the hi and low parts of the counter into the results.
    (inst srlx hi tick 32)
    (inst clruw lo tick)))

#+sparc-v9
(defun read-cycle-counter ()
  "Read the instruction cycle counter available on UltraSparcs.  The
64-bit counter is returned as two 32-bit unsigned integers.  The low 32-bit
result is the first value."
  (read-cycle-counter))
