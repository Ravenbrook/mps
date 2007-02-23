;;; -*- Package: RT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/print.lisp,v 1.3 1994/10/31 04:45:41 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/rt/print.lisp,v 1.3 1994/10/31 04:45:41 ram Exp $
;;;
;;; This file contains temporary printing utilities and similar noise.
;;;
;;; Written by William Lott.
;;;

(in-package "RT")


(define-vop (print)
  (:args (object :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:temporary (:sc any-reg :offset nl0-offset) nl0)
  (:temporary (:sc any-reg :offset lra-offset) lra)
  (:temporary (:scs (sap-reg)) temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:vop-var vop)
  (:generator 0
    (let ((lra-label (gen-label))
	  (cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn cur-nfp nfp-save))
      (inst cal nsp-tn nsp-tn -16)
      (storew object nsp-tn)
      (inst compute-lra-from-code lra code-tn lra-label)
      (inst cai nl0 (make-fixup "_debug_print" :foreign))
      (inst cai temp (make-fixup "call_into_c" :foreign))
      (inst b temp)

      (align vm:lowtag-bits)
      (emit-label lra-label)
      (inst lra-header-word)
      (inst cal nsp-tn nsp-tn 16)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save))
      (move result nl0))))
