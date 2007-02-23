;;; -*- Package: SPARC -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
(ext:file-comment
  "$Header: /project/cmucl/cvsroot/src/compiler/sparc/pred.lisp,v 1.2 1994/10/31 04:46:41 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; $Header: /project/cmucl/cvsroot/src/compiler/sparc/pred.lisp,v 1.2 1994/10/31 04:46:41 ram Exp $
;;;
;;;    This file contains the VM definition of predicate VOPs for the SPARC.
;;;
;;; Written by Rob MacLachlan
;;;
;;; Converted by William Lott.
;;; 

(in-package "SPARC")


;;;; The Branch VOP.

;;; The unconditional branch, emitted when we can't drop through to the desired
;;; destination.  Dest is the continuation we transfer control to.
;;;
(define-vop (branch)
  (:info dest)
  (:generator 5
    (inst b dest)
    (inst nop)))


;;;; Conditional VOPs:

(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg zero null))
	 (y :scs (any-reg descriptor-reg zero null)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:translate eq)
  (:generator 3
    (inst cmp x y)
    (inst b (if not-p :ne :eq) target)
    (inst nop)))
