;;; -*- Package: RT; Log: c.log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/values.lisp,v 1.5 2003/08/03 11:27:46 gerd Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/rt/values.lisp,v 1.5 2003/08/03 11:27:46 gerd Exp $
;;;
;;; This file contains the implementation of unknown-values VOPs.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted for IBM RT by Bill Chiles.
;;;

(in-package "RT")



(define-vop (reset-stack-pointer)
  (:args (ptr :scs (word-pointer-reg)))
  (:generator 1
    (move csp-tn ptr)))

;;; PUSH-VALUES -- VOP.
;;;
;;; Push some values onto the stack, returning the start and number of values
;;; pushed as results.  It is assumed that the Vals are wired to the standard
;;; argument locations.  Nvals is the number of values to push.
;;;
;;; The generator cost is pseudo-random.  We could get it right by defining a
;;; bogus SC that reflects the costs of the memory-to-memory moves for each
;;; operand, but this seems unworthwhile.
;;;
(define-vop (push-values)
  (:args
   (vals :more t))
  (:results
   (start :scs (word-pointer-reg))
   (count :scs (any-reg)))
  (:info nvals)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg)
	       :to (:result 0)
	       :target start)
	      start-temp)
  (:generator 20
    (move start-temp csp-tn)
    (inst cal csp-tn csp-tn (* nvals word-bytes))
    (do ((val vals (tn-ref-across val))
	 (i 0 (1+ i)))
	((null val))
      (let ((tn (tn-ref-tn val)))
	(sc-case tn
	  (descriptor-reg
	   (storew tn start-temp i))
	  (control-stack
	   (load-stack-tn temp tn)
	   (storew temp start-temp i)))))
    (move start start-temp)
    (inst li count (fixnumize nvals))))


;;; VALUES-LIST -- VOP.
;;;
;;; Push a list of values on the stack, returning Start and Count as used in
;;; unknown values continuations.
;;;
(define-vop (values-list)
  (:args (arg :scs (descriptor-reg) :target list))
  (:arg-types list)
  (:policy :fast-safe)
  (:results (start :scs (word-pointer-reg))
	    (count :scs (any-reg)))
  (:temporary (:scs (descriptor-reg) :type list :from (:argument 0)) list)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg) :type random) ndescr)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 0
    (let ((loop (gen-label))
	  (done (gen-label)))

      (move list arg)
      (move start csp-tn)

      (emit-label loop)
      (inst c list null-tn)
      (inst bcx :eq done)
      (loadw temp list cons-car-slot list-pointer-type)
      (loadw list list cons-cdr-slot list-pointer-type)
      (inst cal csp-tn csp-tn word-bytes)
      (storew temp csp-tn -1)
      (test-type list ndescr loop nil list-pointer-type)
      (error-call vop bogus-argument-to-values-list-error list)

      (emit-label done)
      (move count csp-tn)
      (inst s count start))))
