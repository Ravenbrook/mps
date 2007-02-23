;;; -*- Package: C; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/rt/pred.lisp,v 1.2 1994/10/31 04:45:41 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/rt/pred.lisp,v 1.2 1994/10/31 04:45:41 ram Exp $
;;;
;;; This file contains the VM definition of predicate VOPs for the IBM RT.
;;;
;;; Written by Rob MacLachlan
;;; Modified by William Lott and Bill Chiles for the IBM RT.
;;;

(in-package "RT")



;;;; The Branch VOP.

;;; The unconditional branch, emitted when we can't drop through to the desired
;;; destination.  Dest is the label where we want to be.
;;;
(define-vop (branch)
  (:info dest)
  (:generator 5
    (inst b dest)))



;;;; Conditional VOPs:

(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg null))
	 (y :scs (any-reg descriptor-reg null)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:translate eq)
  (:generator 3
    (let ((x-prime (sc-case x
		     ((any-reg descriptor-reg) x)
		     (null null-tn)))
	  (y-prime (sc-case y
		     ((any-reg descriptor-reg) y)
		     (null null-tn))))
      (inst c x-prime y-prime)
      (if not-p
	  (inst bnc :eq target)
	  (inst bc :eq target)))))
