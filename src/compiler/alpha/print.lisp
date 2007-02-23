;;; -*- Package: ALPHA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/alpha/print.lisp,v 1.3 2001/02/20 20:54:32 dtc Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains temporary printing utilities and similar noise.
;;;
;;; Written by William Lott.
;;; Converted by Sean Hallgren.

(in-package "ALPHA")

;;; Some room needs to be allocated on the number stack because call_into_c
;;; pops two register args before calling into C and restores the NSP upon
;;; return.
;;;
(define-vop (print)
  (:args (object :scs (descriptor-reg) :target a0))
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:temporary (:sc any-reg :offset cfunc-offset :target result :to (:result 0))
	      cfunc)
  (:temporary (:sc descriptor-reg :offset nl0-offset :from (:argument 0)) a0)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:vop-var vop)
  (:generator 0
    (let ((cur-nfp (current-nfp-tn vop)))
      (move object a0)
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (inst li (make-fixup "debug_print" :foreign) cfunc)
      (inst li (make-fixup "call_into_c" :foreign) temp)
      (inst lda nsp-tn (- 16) nsp-tn)
      (inst jsr lip-tn temp (make-fixup "call_into_c" :foreign))
      (inst lda nsp-tn 16 nsp-tn)
      (when cur-nfp
	(maybe-load-stack-nfp-tn cur-nfp nfp-save temp))
      (move cfunc result))))
