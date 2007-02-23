;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/sparc/print.lisp,v 1.3 1994/10/31 04:46:41 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/sparc/print.lisp,v 1.3 1994/10/31 04:46:41 ram Exp $
;;;
;;; This file contains VOPs for things like printing during %initial-function
;;; before the world is initialized.
;;;
;;; Written by William Lott.

(in-package "SPARC")


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
      (inst li cfunc (make-fixup (extern-alien-name "debug_print") :foreign))
      (inst li temp (make-fixup (extern-alien-name "call_into_c") :foreign))
      (inst jal lip temp)
      (inst nop)
      (when cur-nfp
	(load-stack-tn cur-nfp nfp-save))
      (move result nl0))))
