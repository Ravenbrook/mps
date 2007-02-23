;;; -*- Package: HPPA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/hppa/print.lisp,v 1.2 1994/10/31 04:42:45 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains temporary printing utilities and similar noise.
;;;
;;; Written by William Lott.

(in-package "HPPA")


(define-vop (print)
  (:args (object :scs (descriptor-reg) :target arg))
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:temporary (:sc non-descriptor-reg :offset cfunc-offset) cfunc)
  (:temporary (:sc non-descriptor-reg :offset nl0-offset :from (:argument 0))
	      arg)
  (:temporary (:sc non-descriptor-reg :offset nl4-offset :to (:result 0))
	      res)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:generator 0
    (let ((cur-nfp (current-nfp-tn vop)))
      (move object arg)
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      ;; Allocate 64 bytes, the minimum stack size.
      (inst addi 64 nsp-tn nsp-tn)
      (inst li (make-fixup "debug_print" :foreign) cfunc)
      (let ((fixup (make-fixup "call_into_c" :foreign)))
	(inst ldil fixup temp)
	(inst ble fixup c-text-space temp :nullify t)
	(inst nop))
      (inst addi -64 nsp-tn nsp-tn)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save))
      (move res result))))
