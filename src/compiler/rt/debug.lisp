;;; -*- Package: RT; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/debug.lisp,v 1.5 1994/10/31 04:45:41 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/rt/debug.lisp,v 1.5 1994/10/31 04:45:41 ram Exp $
;;;
;;; Compiler support for the new whizzy debugger.
;;;
;;; Written by William Lott.
;;; Converted to RT by Bill Chiles.
;;;

(in-package "RT")


(defknown di::current-sp () system-area-pointer (movable flushable))
(defknown di::current-fp () system-area-pointer (movable flushable))
(defknown di::stack-ref (system-area-pointer index) t (flushable))
(defknown di::%set-stack-ref (system-area-pointer index t) t (unsafe))
(defknown di::lra-code-header (t) t (movable flushable))
(defknown di::function-code-header (t) t (movable flushable))
(defknown di::make-lisp-obj ((unsigned-byte 32)) t (movable flushable))
(defknown di::get-lisp-obj-address (t) (unsigned-byte 32) (movable flushable))

(define-vop (debug-cur-sp)
  (:translate di::current-sp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 1
    (move res csp-tn)))

(define-vop (debug-cur-fp)
  (:translate di::current-fp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 1
    (move res cfp-tn)))


(define-vop (read-control-stack-c)
  (:policy :fast-safe)
  (:translate di::stack-ref)
  (:args (base :scs (sap-reg)))
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:arg-types system-area-pointer (:constant (unsigned-byte 13)))
  (:info offset)
  (:generator 5
    (inst l result base (* offset word-bytes))))

(define-vop (read-control-stack)
  (:policy :fast-safe)
  (:translate di::stack-ref)
  (:args (object :scs (sap-reg) :target base)
	 (offset :scs (any-reg)))
  (:results (result :scs (descriptor-reg)))
  (:arg-types system-area-pointer positive-fixnum)
  (:result-types *)
  (:temporary (:scs (sap-reg) :from (:argument 0) :to :eval) base)
  (:generator 7
    (move base object)
    (inst a base offset)
    (inst l result base 0)))

(define-vop (write-control-stack-c)
  (:policy :fast-safe)
  (:translate di::%set-stack-ref)
  (:args (base :scs (sap-reg))
	 (data :scs (descriptor-reg) :target result :to (:result 0)))
  (:arg-types system-area-pointer (:constant (unsigned-byte 13)) *)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:info offset)
  (:generator 5
    (inst st data base (* offset word-bytes))
    (move result data)))

(define-vop (write-control-stack)
  (:policy :fast-safe)
  (:translate di::%set-stack-ref)
  (:args (object :scs (sap-reg) :target base)
	 (offset :scs (any-reg))
	 (data :scs (descriptor-reg) :target result))
  (:arg-types system-area-pointer positive-fixnum *)
  (:temporary (:scs (sap-reg) :from (:argument 0) :to :eval) base)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 7
    (move base object)
    (inst cas base base offset)
    (inst st data base 0)
    (move result data)))


(define-vop (code-from-mumble)
  (:policy :fast-safe)
  (:args (thing :scs (descriptor-reg) :target code))
  (:results (code :scs (descriptor-reg)))
  (:temporary (:scs (sap-reg)) temp)
  (:variant-vars lowtag)
  (:generator 5
    (let ((bogus (gen-label))
	  (done (gen-label)))
      (loadw temp thing 0 lowtag)
      (inst sr temp vm:type-bits)
      (inst bc :eq bogus)
      (inst sl temp (1- (integer-length vm:word-bytes)))
      (unless (= lowtag vm:other-pointer-type)
	(inst cal temp temp (- lowtag vm:other-pointer-type)))
      (move code thing)
      (inst s code temp)
      (emit-label done)
      (assemble (*elsewhere*)
	(emit-label bogus)
	(inst bx done)
	(move code null-tn)))))

(define-vop (code-from-lra code-from-mumble)
  (:translate di::lra-code-header)
  (:variant vm:other-pointer-type))

(define-vop (code-from-function code-from-mumble)
  (:translate di::function-code-header)
  (:variant vm:function-pointer-type))

(define-vop (di::make-lisp-obj)
  (:policy :fast-safe)
  (:translate di::make-lisp-obj)
  (:args (value :scs (unsigned-reg) :target result))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:generator 1
    (move result value)))

(define-vop (di::get-lisp-obj-address)
  (:policy :fast-safe)
  (:translate di::get-lisp-obj-address)
  (:args (thing :scs (descriptor-reg) :target result))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (move result thing)))
