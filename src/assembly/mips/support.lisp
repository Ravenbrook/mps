;;; -*- Package: MIPS -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/assembly/mips/support.lisp,v 1.15 1994/10/31 04:56:40 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains the machine specific support routines needed by
;;; the file assembler.
;;;
(in-package "MIPS")

(def-vm-support-routine generate-call-sequence (name style vop)
  (ecase style
    (:raw
     (let (#+gengc (ra (make-symbol "RA")))
       (values
	`((inst jal #+gengc ,ra (make-fixup ',name :assembly-routine))
	  (inst nop))
	`(#+gengc
	  (:temporary (:sc descriptor-reg :from (:eval 0) :to (:eval 1)
		       :offset ra-offset)
		      ,ra)))))
    (:full-call
     (let ((temp (make-symbol "TEMP"))
	   (nfp-save (make-symbol "NFP-SAVE"))
	   #-gengc (lra (make-symbol "LRA"))
	   #+gengc (ra (make-symbol "RA")))
       (values
	`((let ((lra-label (gen-label))
		(cur-nfp (current-nfp-tn ,vop)))
	    (when cur-nfp
	      (store-stack-tn ,nfp-save cur-nfp))
	    #-gengc
	    (inst compute-lra-from-code ,lra code-tn lra-label ,temp)
	    (note-next-instruction ,vop :call-site)
	    (inst #-gengc j #+gengc jal (make-fixup ',name :assembly-routine))
	    (inst nop)
	    (emit-return-pc lra-label)
	    (note-this-location ,vop :single-value-return)
	    (without-scheduling ()
	      (move csp-tn ocfp-tn)
	      (inst nop))
	    #-gengc
	    (inst compute-code-from-lra code-tn code-tn
		  lra-label ,temp)
	    #+gengc
	    (inst compute-code-from-ra code-tn ra-tn lra-label ,temp)
	    (when cur-nfp
	      (load-stack-tn cur-nfp ,nfp-save))))
	`((:temporary (:scs (non-descriptor-reg) :from (:eval 0) :to (:eval 1))
		      ,temp)
	  #-gengc
	  (:temporary (:sc descriptor-reg :offset lra-offset
		       :from (:eval 0) :to (:eval 1))
		      ,lra)
	  #+gengc
	  (:temporary (:sc any-reg :offset ra-offset
		       :from (:eval 0) :to (:eval 1))
		      ,ra)
	  #+gengc (:ignore ,ra)
	  (:temporary (:scs (control-stack) :offset nfp-save-offset)
		      ,nfp-save)
	  (:save-p t)))))
    (:none
     (values
      `((inst j (make-fixup ',name :assembly-routine))
	(inst nop))
      nil))))


(def-vm-support-routine generate-return-sequence (style)
  (ecase style
    (:raw
     `((inst j #-gengc lip-tn #+gengc ra-tn)
       (inst nop)))
    (:full-call
     #-gengc
     `((lisp-return (make-random-tn :kind :normal
				    :sc (sc-or-lose
					 'descriptor-reg)
				    :offset lra-offset)
		    lip-tn :offset 2))
     #+gengc
     `((inst addu lip-tn ra-tn (* word-bytes 2))
       (inst j lip-tn)
       (inst nop)))
    (:none)))
