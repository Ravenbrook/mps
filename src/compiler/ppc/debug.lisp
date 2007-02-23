;;; -*- Package: PPC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/ppc/debug.lisp,v 1.1 2001/02/11 14:22:04 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; Compiler support for the new whizzy debugger.
;;;
;;; Written by William Lott.
;;; 
(in-package "PPC")

(defknown di::current-sp () system-area-pointer (movable flushable))
(defknown di::current-fp () system-area-pointer (movable flushable))
(defknown di::stack-ref (system-area-pointer index) t (flushable))
(defknown di::%set-stack-ref (system-area-pointer index t) t (unsafe))
(defknown di::lra-code-header (t) t (movable flushable))
(defknown di::function-code-header (t) t (movable flushable))
(defknown di::make-lisp-obj ((unsigned-byte 32)) t (movable flushable))
(defknown di::get-lisp-obj-address (t) (unsigned-byte 32) (movable flushable))
(defknown di::function-word-offset (function) index (movable flushable))

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

(define-vop (read-control-stack)
  (:translate kernel:stack-ref)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
	 (offset :scs (any-reg)))
  (:arg-types system-area-pointer positive-fixnum)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 5
    (inst lwzx result sap offset)))

(define-vop (write-control-stack)
  (:translate kernel:%set-stack-ref)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
	 (offset :scs (any-reg))
	 (value :scs (descriptor-reg) :target result))
  (:arg-types system-area-pointer positive-fixnum *)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 5
    (inst stwx value sap offset)
    (move result value)))

(define-vop (code-from-mumble)
  (:policy :fast-safe)
  (:args (thing :scs (descriptor-reg)))
  (:results (code :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:variant-vars lowtag)
  (:generator 5
    (let ((bogus (gen-label))
	  (done (gen-label)))
      (loadw temp thing 0 lowtag)
      (inst srwi temp temp vm:type-bits)
      (inst cmpwi temp 0)
      (inst slwi temp temp (1- (integer-length vm:word-bytes)))
      (inst beq bogus)
      (unless (= lowtag vm:other-pointer-type)
	(inst addi temp temp (- lowtag vm:other-pointer-type)))
      (inst sub code thing temp)
      (emit-label done)
      (assemble (*elsewhere*)
	(emit-label bogus)
	(move code null-tn)
	(inst b done)))))

(define-vop (code-from-lra code-from-mumble)
  (:translate di::lra-code-header)
  (:variant vm:other-pointer-type))

(define-vop (code-from-function code-from-mumble)
  (:translate di::function-code-header)
  (:variant vm:function-pointer-type))

(define-vop (make-lisp-obj)
  (:policy :fast-safe)
  (:translate di::make-lisp-obj)
  (:args (value :scs (unsigned-reg) :target result))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:generator 1
    (move result value)))

(define-vop (get-lisp-obj-address)
  (:policy :fast-safe)
  (:translate di::get-lisp-obj-address)
  (:args (thing :scs (descriptor-reg) :target result))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (move result thing)))


(define-vop (function-word-offset)
  (:policy :fast-safe)
  (:translate di::function-word-offset)
  (:args (fun :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 5
    (loadw res fun 0 function-pointer-type)
    (inst srwi res res vm:type-bits)))
