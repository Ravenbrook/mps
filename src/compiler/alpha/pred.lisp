;;; -*- Package: ALPHA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/alpha/pred.lisp,v 1.2 1994/10/31 04:39:51 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;;    This file contains the VM definition of predicate VOPs for the Alpha.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted by Sean Hallgren.
;;; 

(in-package "ALPHA")


;;;; The Branch VOP.

;;; The unconditional branch, emitted when we can't drop through to the desired
;;; destination.  Dest is the continuation we transfer control to.
;;;
(define-vop (branch)
  (:info dest)
  (:generator 5
    (inst br zero-tn dest)))


;;;; Conditional VOPs:

(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg zero null))
	 (y :scs (any-reg descriptor-reg zero null)))
  (:conditional)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:info target not-p)
  (:policy :fast-safe)
  (:translate eq)
  (:generator 3
    (inst cmpeq x y temp)
    (if not-p
	(inst beq temp target)
	(inst bne temp target))))
