;;; -*- Package: PPC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/assembly/ppc/support.lisp,v 1.3 2005/04/08 04:11:01 rtoy Exp $")
;;;
;;; **********************************************************************
;;;
(in-package "PPC")

(def-vm-support-routine generate-call-sequence (name style vop)
  (ecase style
    (:raw
     (values 
      `((inst bla (make-fixup ',name :assembly-routine)))
      `()))
    (:full-call
     (let ((temp (make-symbol "TEMP"))
	   (nfp-save (make-symbol "NFP-SAVE"))
	   (lra (make-symbol "LRA")))
       (values
	`((let ((lra-label (gen-label))
		(cur-nfp (current-nfp-tn ,vop)))
	    (when cur-nfp
	      (store-stack-tn ,nfp-save cur-nfp))
	    (inst compute-lra-from-code ,lra code-tn lra-label ,temp)
	    (note-next-instruction ,vop :call-site)
            (inst ba (make-fixup ',name :assembly-routine))
	    (emit-return-pc lra-label)
	    (note-this-location ,vop :single-value-return)
	    (without-scheduling ()
				(move csp-tn ocfp-tn)
				(inst nop))
	    (inst compute-code-from-lra code-tn code-tn
		  lra-label ,temp)
	    (when cur-nfp
	      (load-stack-tn cur-nfp ,nfp-save))))
	`((:temporary (:scs (non-descriptor-reg) :from (:eval 0) :to (:eval 1))
	   ,temp)
	  (:temporary (:sc descriptor-reg :offset lra-offset
		       :from (:eval 0) :to (:eval 1))
	   ,lra)
	  (:temporary (:scs (control-stack) :offset nfp-save-offset)
	   ,nfp-save)
	  (:save-p :compute-only)))))
    (:none
     (values 
      `((inst ba  (make-fixup ',name :assembly-routine)))
      `()))))

(def-vm-support-routine generate-return-sequence (style)
  (ecase style
    (:raw
     `((inst blr)))
    (:full-call
     `((lisp-return (make-random-tn :kind :normal
				    :sc (sc-or-lose 'descriptor-reg *backend*)
				    :offset lra-offset)
		    (make-random-tn :kind :normal
		                    :sc (sc-or-lose 'interior-reg *backend*)
				    :offset lip-offset)
		    :offset 2)))
    (:none)))
