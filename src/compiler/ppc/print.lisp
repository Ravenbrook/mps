;;; -*- Package: PPC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/ppc/print.lisp,v 1.2 2004/07/25 18:15:52 pmai Exp $
;;;
;;; This file contains VOPs for things like printing during %initial-function
;;; before the world is initialized.
;;;
;;; Written by William Lott.

(in-package "PPC")


(define-vop (print)
  (:args (object :scs (descriptor-reg any-reg) :target nl0))
  (:results (result :scs (descriptor-reg)))
  (:save-p t)
  (:temporary (:sc any-reg :offset nl0-offset :from (:argument 0)) nl0)
  (:temporary (:sc any-reg :offset cfunc-offset) cfunc)
  (:temporary (:sc interior-reg :offset lip-offset) lip)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc control-stack :offset nfp-save-offset) nfp-save)
  (:vop-var vop)
  (:generator 100
    (let ((cur-nfp (current-nfp-tn vop)))
      (when cur-nfp
	(store-stack-tn nfp-save cur-nfp))
      (move nl0 object)
      (inst lr temp  (make-fixup (extern-alien-name "call_into_c") :foreign))
      (inst mr lip temp)
      (inst mtctr lip)
      (inst lr cfunc (make-fixup (extern-alien-name "debug_print") :foreign))
      (inst bctrl)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save))
      (move result nl0))))
