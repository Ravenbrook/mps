;;; -*- Package: RT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/assembly/rt/support.lisp,v 1.3 1994/10/31 04:57:00 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/assembly/rt/support.lisp,v 1.3 1994/10/31 04:57:00 ram Exp $
;;;
;;; This file contains the machine specific support routines needed by
;;; the file assembler.
;;;

(in-package "RT")

(def-vm-support-routine generate-call-sequence (name style vop)
  (ecase style
    (:raw
     (values
      `((inst bala (make-fixup ',name :assembly-routine)))
      nil))
    (:full-call
     (let ((nfp-save (make-symbol "NFP-SAVE"))
	   (lra (make-symbol "LRA")))
       (values
	`((let ((lra-label (gen-label))
		(cur-nfp (current-nfp-tn ,vop)))
	    (when cur-nfp
	      (store-stack-tn cur-nfp ,nfp-save))
	    (inst compute-lra-from-code ,lra code-tn lra-label)
	    ;; This absolute branch trashes the LIP register, but we don't use
	    ;; it when calling assembly routines.
	    (inst bala (make-fixup ',name :assembly-routine))
	    (emit-return-pc lra-label)
	    (note-this-location ,vop :unknown-return)
	    ;; Don't use MOVE.  Use a known 32-bit long instruction, so the
	    ;; returner can know how many bytes we used here in the
	    ;; multiple-value return case.  The returner wants to add a known
	    ;; quantity to LRA indicating how many values it returned.
	    (inst cal csp-tn ocfp-tn 0)
	    (inst compute-code-from-lra code-tn code-tn lra-label)
	    (when cur-nfp
	      (load-stack-tn cur-nfp ,nfp-save))))
	`((:temporary (:sc descriptor-reg :offset lra-offset
			   :from (:eval 0) :to (:eval 1))
		      ,lra)
	  (:temporary (:scs (control-stack) :offset nfp-save-offset)
		      ,nfp-save)))))
    (:none
     (values
      ;; This absolute branch trashes the LIP register, but we don't use it
      ;; when calling assembly routines.
      `((inst bala (make-fixup ',name :assembly-routine)))
      nil))))


(def-vm-support-routine generate-return-sequence (style)
  (ecase style
    (:raw
     `((inst b lip-tn)))
    (:full-call
     `((lisp-return lra-tn lip-tn :offset 1)))
    (:none)))
