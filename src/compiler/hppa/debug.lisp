;;; -*- Package: HPPA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/hppa/debug.lisp,v 1.2 1994/10/31 04:42:45 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Compiler support for the new whizzy debugger.
;;;
;;; Written by William Lott.
;;; 
(in-package "HPPA")


(define-vop (debug-cur-sp)
  (:translate current-sp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 1
    (move csp-tn res)))

(define-vop (debug-cur-fp)
  (:translate current-fp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 1
    (move cfp-tn res)))

(define-vop (read-control-stack)
  (:translate stack-ref)
  (:policy :fast-safe)
  (:args (object :scs (sap-reg))
	 (offset :scs (any-reg)))
  (:arg-types system-area-pointer positive-fixnum)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 5
    (inst ldwx offset object result)))

(define-vop (read-control-stack-c)
  (:translate stack-ref)
  (:policy :fast-safe)
  (:args (object :scs (sap-reg)))
  (:info offset)
  (:arg-types system-area-pointer (:constant (signed-byte 12)))
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 4
    (inst ldw (* offset word-bytes) object result)))

(define-vop (write-control-stack)
  (:translate %set-stack-ref)
  (:policy :fast-safe)
  (:args (object :scs (sap-reg) :target sap)
	 (offset :scs (any-reg))
	 (value :scs (descriptor-reg) :target result))
  (:arg-types system-area-pointer positive-fixnum *)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:temporary (:scs (sap-reg) :from (:argument 1)) sap)
  (:generator 2
    (inst add object offset sap)
    (inst stw value 0 sap)
    (move value result)))

(define-vop (write-control-stack-c)
  (:translate %set-stack-ref)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
	 (value :scs (descriptor-reg) :target result))
  (:info offset)
  (:arg-types system-area-pointer (:constant (signed-byte 12)) *)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 1
    (inst stw value (* offset word-bytes) sap)
    (move value result)))

(define-vop (code-from-mumble)
  (:policy :fast-safe)
  (:args (thing :scs (descriptor-reg) :to :save))
  (:results (code :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:variant-vars lowtag)
  (:generator 5
    (loadw temp thing 0 lowtag)
    (inst srl temp type-bits temp)
    (inst comb := zero-tn temp done)
    (move null-tn code)
    (inst sll temp (1- (integer-length word-bytes)) temp)
    (unless (= lowtag other-pointer-type)
      (inst addi (- lowtag vm:other-pointer-type) temp temp))
    (inst sub thing temp code)
    DONE))

(define-vop (code-from-lra code-from-mumble)
  (:translate lra-code-header)
  (:variant other-pointer-type))

(define-vop (code-from-function code-from-mumble)
  (:translate function-code-header)
  (:variant function-pointer-type))

(define-vop (make-lisp-obj)
  (:policy :fast-safe)
  (:translate make-lisp-obj)
  (:args (value :scs (unsigned-reg) :target result))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:generator 1
    (move value result)))

(define-vop (get-lisp-obj-address)
  (:policy :fast-safe)
  (:translate get-lisp-obj-address)
  (:args (thing :scs (descriptor-reg) :target result))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (move thing result)))

(define-vop (function-word-offset)
  (:policy :fast-safe)
  (:translate function-word-offset)
  (:args (fun :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 5
    (loadw res fun 0 function-pointer-type)
    (inst srl res type-bits res)))
